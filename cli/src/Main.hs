{-# LANGUAGE CPP #-}

module Main where

#if defined(mingw32_HOST_OS)
import           Windows                  (runDevevIO)
#elif (darwin_HOST_OS)
import           Darwin                   (runDevevIO)
#else
import           Linux                    (runDevevIO)
#endif

import           Data.List                (maximum, nubBy)
import qualified Data.Text                as Text

import           System.Directory         (createDirectoryIfMissing, doesFileExist)
import           System.Exit              (ExitCode (..))

import           Cleff
import           Cleff.Error

import           Control.Concurrent.Async (concurrently, wait, withAsync)
import           Control.Concurrent.STM   (TChan, dupTChan, readTChan, writeTChan)
import           Control.Monad.Logger     (LogLine, MonadLogger, monadLoggerLog, runFileLoggingT,
                                           runStdoutLoggingT)
import           Devenv
import           Parser                   (Command (..), parseCmd)
import           Types
import           Util

findInstance :: Devenv :> es => InstanceName -> Eff es (Maybe DevenvInstance)
findInstance name = find ((== name) . _name) <$> listInstances

devenv :: [Logger, Devenv, IOE] :>> es
       => Errors '[NixError, Panic] :>> es
       => Command -> Eff es ()
devenv = \case
    CommandStatus            ->
        getStatus >>= \case
            DevenvNotInstalled -> printErrExit (ExitFailure 1)
                "The devenv controller is not installed. Please run 'devenv start' to install it."
            DevenvInstalled status -> do
                printLnOut $ "Controller Status: " <> show status

                instances <- listInstances
                if not (null instances)
                    then printLnOut $ showTable "\t"
                             $ [ "NAME", "STATUS" ]
                             : [ [getInstanceName _name, show _status] | DevenvInstance{..} <- instances ]
                    else printLnOut "No devenv instances found!"

    CommandStart            -> do
        let doStart status = do
                printLnOut "Starting controller..."
                start (status == Running)
        getStatus >>= \case
            DevenvInstalled status -> doStart status
            DevenvNotInstalled -> do
                initCtrl
                getStatus >>= \case
                    DevenvNotInstalled -> throwError $ Panic $
                        UnrecoverableError "Couldn't install devenv controller!"
                    DevenvInstalled status -> doStart status

    CommandInstall name flakes -> do
        whenJustM (findInstance name) $ \_ ->
            printErrExit (ExitFailure 1) $
                "Devenv instance " <> getInstanceName name <> " already exists"

        instanceDir <- getPath DirCtrl $
            fromPathSegments ["instances", getInstanceName name]
        instanceFlake <- getPath DirCtrl $
            fromPathSegments ["instances", getInstanceName name, "flake.nix"]

        devenvModule <- getDevenvModule

        liftIO $ do
            createDirectoryIfMissing True instanceDir
            writeFileText instanceFlake $ mkNixFlake name (addNamesFromURL flakes) devenvModule

        let flakePath = "/instances/" <> getInstanceName name
            drvPath = "nixosConfigurations.default.config.system.build.systemd-tarball"
            updateCmd = "nix flake update " <> flakePath
            -- impure because we need pkgs.replaceDependency
            buildCmd = "nix build --impure --print-out-paths --no-link " <> flakePath <> "#" <> drvPath

        _ <- either (throwError . NixError) return =<< runCtrlNixCmd True updateCmd
        drv <- either
            (throwError . NixError)
            (rethrowPanic @ParseException . parse_ @StorePath)
                =<< runCtrlNixCmd True buildCmd

        rootfsFile <- getPath DirCtrl $
            unStorePath drv `joinPaths` fromPathSegments ["tarball", "nixos-wsl-x86_64-linux.tar.gz" ]

        unlessM (liftIO $ doesFileExist rootfsFile) $
            throwError $ Panic $ NixError $
                "Nix build didn't build " <> toText rootfsFile <> " as expected."

        installRootfs name rootfsFile
        printLnOut $ "Successfully installed " <> getInstanceName name

    CommandUninstall name   -> do
        whenNothingM_ (findInstance name) $
            printErrExit (ExitFailure 1) $
                "Devenv instance " <> getInstanceName name <> " does not exists"

        uninstallInstance name
        void $ either (throwError . NixError) return
            =<< runCtrlNixCmd False ("rm -r /instances/" <> getInstanceName name)

    CommandUpdate name -> do
        unlessM isCtrlRunning $
            printErrExit (ExitFailure 1) "Devenv controller is not running..."

        whenNothingM_ (findInstance name) $
            printErrExit (ExitFailure 1) $
                "Devenv instance " <> getInstanceName name <> " does not exists"

        let flakePath = "/instances/" <> getInstanceName name
            updateCmd = "nix flake update " <> flakePath
            drvPath   = "nixosConfigurations.default.config.system.build.toplevel"
            -- impure because we need pkgs.replaceDependency
            buildCmd  = "nix build --impure --print-out-paths --no-link " <> flakePath <> "#" <> drvPath

        _ <- either (throwError . NixError) return =<< runCtrlNixCmd True updateCmd
        printLnOut "Building new system..."
        drv <- either
            (throwError . NixError)
            (rethrowPanic @ParseException . parse_ @StorePath)
            . logTraceShowId "store path"
                =<< runCtrlNixCmd True buildCmd

        printLnOut "Updating system..."

        let switcherPath = fromPathSegments @Abs [] `joinPaths`
                           unStorePath drv `joinPaths`
                           fromPathSegments ["bin", "switch-to-configuration"]
            installCmd = toUnixPath switcherPath <> " switch"

        _ <- either (throwError . NixError) return
            =<< runInstanceCmd True name installCmd

        _ <- either (throwError . NixError) return
            =<< runInstanceCmd True name "nix-env -p /nix/var/nix/profiles/system --set /run/current-system"

        printLnOut "System update successfull!"


    CommandRun name         -> do
        unlessM isCtrlRunning $
            printErrExit (ExitFailure 1) "Devenv controller is not running..."

        findInstance name >>= \case
            Nothing -> printErrExit (ExitFailure 1) $
                "Devenv instance " <> getInstanceName name <> " does not exists"
            Just i -> runInstance i

isCtrlRunning :: Devenv :> es => Eff es Bool
isCtrlRunning =
    getStatus <&> \case
        DevenvInstalled status -> status == Running
        _                      -> False

mkNixFlake :: InstanceName -> [DevenvFlake Text] -> Text -> Text
mkNixFlake instanceName inputs devenvModule = unlines
    [ "{"
    , "  description = \"Devenv flake for " <> getInstanceName instanceName <> "\";"
    , "  inputs = {"
    , "    __devenv.url = \"gitlab:afojhr/nixos-devenv\";"
    , forEachUnline uniqueInputs $ \fl ->
      "    " <> _flakeMetadata fl <> ".url = \"" <> _flakeURL fl <> "\";"
    , optionalStr (listToMaybe inputs) $ \fl ->
      "    nixpkgs.follows = \"" <> _flakeMetadata fl <> "/nixpkgs\";"
    , "  };"
    , "  outputs = inputs: {"
    , "    nixosConfigurations.default = inputs.nixpkgs.lib.nixosSystem {"
    , "      system = \"x86_64-linux\";"
    -- , "      specialArgs = { inherit inputs; };"
    , "      modules = ["
    , "        inputs.__devenv.nixosModules." <> devenvModule
    , forEachUnline inputs $ \fl ->
      "        inputs." <> _flakeMetadata fl <> ".nixosModules." <> _moduleName fl
    , "      ];"
    , "    };"
    , "  };"
    , "}"
    ]

    where
        forEachUnline xs f = unlines $ map f xs
        optionalStr may f = maybe mempty f may

        uniqueInputs = nubBy (\a b -> _flakeMetadata a == _flakeMetadata b) inputs


showTable :: Text -> [[Text]] -> Text
showTable sep t = unlines (map mkLine t) where
    colWidths = map (maximum . map Text.length)
              $ transpose t

    mkLine = Text.intercalate sep
           . zipWith (`Text.justifyLeft` ' ') colWidths


main :: IO ()
main = do

    logFile <- runDevenv $ getPath DirState (fromPathSegments [_DEVENV_APP_NAME <> ".log"])

    let stdoutSink = runStdoutLoggingT . unTChanLoggingT =<< atomically (dupTChan logChan)
        fileSink = runFileLoggingT logFile . unTChanLoggingT =<< atomically (dupTChan logChan)

    withAsync (stdoutSink `concurrently` fileSink) $ \action -> do
        runDevenv . devenv =<< parseCmd
        atomically $ writeTChan logChan Nothing
        void $ wait action

    where
        runDevenv = runIOE
                  . interpretLogger
                  . pluckEffects @IOE @(Errors '[Panic, DriverException, NixError, UserError])
                  . runDevevIO

unTChanLoggingT :: (MonadLogger m, MonadIO m) => TChan (Maybe LogLine) -> m ()
unTChanLoggingT chan = fix $ \loop ->
    atomically (readTChan chan) >>= \case
        Just (loc,src,lvl,msg) -> monadLoggerLog loc src lvl msg >> loop
        Nothing                -> pass

