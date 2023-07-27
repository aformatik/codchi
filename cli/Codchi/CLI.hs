module Codchi.CLI where

import Codchi.Parser
import Codchi.Types
import Options.Applicative

-- import qualified Data.ByteString.Char8 as BS
import Data.Version (showVersion)
-- import Language.Haskell.TH
import Paths_codchi (version)
-- import System.Directory (doesFileExist, getCurrentDirectory, getDirectoryContents)

parseCmd :: IO Command
parseCmd =
    execParser $
        info
            ( cmdP
                <**> helper
                <**> simpleVersioner (showVersion version)
            )
            ( fullDesc
                <> header "CODe maCHInes - Declarative, Reproducible, Cross Platform Development Environments as Code"
            )

data Command
    = CmdStart
    | CmdStatus
    | CmdAddCa CodchiName FilePath
    | CmdInit CodchiName
    | CmdAddModule CodchiName ModuleConfigureArgs
    | CmdRemoveModule CodchiName CodchiName
    | CmdSetFollows CodchiName NixpkgsFollows
    | CmdRebuild CodchiName
    | CmdRun CodchiName WithTerminal [Text]
    | CmdUninstall CodchiName
    deriving (Eq, Show)

type WithTerminal = Bool

cmdP :: Parser Command
cmdP =
    subparser
        ( command "start" (info startP (progDesc "Start codchi controller"))
            <> command "status" (info statusP (progDesc "Show codchi status"))
            <> command "add-ca" (info addCaP (progDesc "Add self signed CA certificate. Useful for adding modules over self signed https."))
        )
        <|> subparser (instanceCmdP <> hidden <> commandGroup "Code machine commands")
  where
    statusP = pure CmdStatus
    startP = pure CmdStart
    addCaP =
        CmdAddCa
            <$> argument parseable (metavar "NAME")
            <*> argument str (metavar "FILE.crt")

instanceCmdP :: Mod CommandFields Command
instanceCmdP =
    command "init" (info (CmdInit <$> name) (progDesc "Initialize a code machine"))
        <> command
            "add-module"
            ( info
                (CmdAddModule <$> name <*> configP <**> helper)
                (progDesc "Add MODULE_NAME to CODE_MACHINE")
            )
        <> command
            "remove-module"
            ( info
                (CmdRemoveModule <$> name <*> moduleName)
                (progDesc "Remove MODULE_NAME from CODE_MACHINE")
            )
        <> command "set-follows" (info setFollowsP (progDesc "Configure if a code machine follows codchis' nixpkgs or yours"))
        <> command
            "rebuild"
            ( info
                (CmdRebuild <$> name)
                (progDesc "(Re-)build a code machine. On first run this will register with your specific driver.")
            )
        <> command
            "run"
            ( info
                (runP <**> helper)
                (progDesc "Open a shell (empty CMD) or run `CMD ARGS...` in a code machine")
            )
        <> command
            "uninstall"
            ( info
                (CmdUninstall <$> name)
                (progDesc "Delete and unregister a code machine. THIS WILL DELETE ALL DATA OF THIS CODE MACHINE!")
            )
  where
    name = argument parseable (metavar "CODE_MACHINE")
    moduleName = argument parseable (metavar "MODULE_NAME")
    setFollowsP =
        CmdSetFollows
            <$> name
            <*> ( ( CodchiNixpkgs
                        <$ flag'
                            ()
                            ( long "codchi"
                                <> short 'c'
                                <> help "Follow codchis' provided nixpkgs for this code machine"
                            )
                  )
                    <|> ModuleNixpkgs
                        <$> option
                            parseable
                            ( long "module"
                                <> short 'm'
                                <> metavar "MODULE_NAME"
                                <> help "Follow the nixpkgs input from MODULE_NAME. The system will only be updated when MODULE_NAMEs' flake is updated."
                            )
                )
    runP =
        CmdRun
            <$> name
            <*> ( not
                    <$> switch
                        ( long "no-terminal"
                            <> short 's'
                            <> help "Whether to run CMD without a terminal. Usefull if running graphical apps."
                        )
                )
            <*> many (argument str (metavar "CMD ARGS..."))

data ModuleConfigureArgs = ModuleConfigureArgs
    { moduleName :: CodchiName
    , uri :: Text
    , branchCommit :: Maybe BranchCommit
    , moduleType :: ModuleType
    -- ^ One can either use an nixosModule inside a flake or directly import a .nix file
    }
    deriving (Eq, Show)

configP :: Parser ModuleConfigureArgs
configP =
    ModuleConfigureArgs
        <$> argument parseable (metavar "MODULE_NAME")
        <*> strOption
            ( long "uri"
                <> short 'u'
                <> metavar "URI"
                <> help "URI of the module. Can be an absolute file path or a git repository url (https or ssh supported)."
            )
        <*> optional
            ( BranchCommit
                <$> strOption
                    ( long "branch"
                        <> short 'b'
                        <> metavar "BRANCH_OR_TAG"
                        <> help "Specify the git branch or tag. Only applicable when URI points to a git repository."
                    )
                <*> optional
                    ( strOption
                        ( long "commit"
                            <> short 'c'
                            <> metavar "COMMIT"
                            <> help "Specify the git commit inside BRANCH_OR_TAG. Only applicable when URI points to a git repository."
                        )
                    )
            )
        <*> nixosModuleP
  where
    nixosModuleP =
        ( FlakeModule
            <$> option
                parseable
                ( long "module"
                    <> short 'm'
                    <> metavar "FLAKE_MODULE"
                    <> help "Name of NixOS Module when using flakes. Must be under `nixosModules`."
                )
            <*> optional
                ( option
                    parseable
                    ( long "flake-dir"
                        <> short 'd'
                        <> metavar "FLAKE_DIR"
                        <> help "Path to directory of flake.nix relative to the git repository. Only applicable if using flakes."
                    )
                )
        )
            <|> ( LegacyModule
                    <$> option
                        parseable
                        ( long "file"
                            <> short 'f'
                            <> metavar "FILE"
                            <> help "Path to NixOS configuration file inside URI. Only applicable if not using flakes."
                        )
                )

-- gitHash :: Q Exp
-- gitHash =
--     stringE
--         =<< runIO
--             ( do
--                 putStr "Current Directory: "
--                 getCurrentDirectory >>= print
--                 putStr "Directory contents: "
--                 getCurrentDirectory >>= getDirectoryContents >>= print
--                 let tryReadRef prefix = do
--                         let path = prefix <> ".git/refs/heads/master"
--                         exists <- doesFileExist path
                        -- if exists
                        --     then Just . decodeUtf8 . BS.strip <$> readFileBS path
                        --     else return Nothing
                -- ref <- listToMaybe . catMaybes <$> traverse tryReadRef ["", "../"]
--                 case ref of
--                     Nothing -> error "Couldn't find .git folder to read latest commit hash"
--                     Just r -> return r
--             )
