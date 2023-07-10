module Parser where

import Data.Attoparsec.Text (parseOnly)
import Options.Applicative

import Types
import Util                 (Parseable, parser)

data Command
    = CommandStatus
    | CommandStart
    | CommandInstall InstanceName [CodchiFlake ()]
    | CommandUninstall InstanceName
    | CommandRun Bool InstanceName [Text]
    | CommandUpdate InstanceName
  deriving (Eq, Show)

cmdP :: Parser Command
cmdP = subparser
     $ command "status"     (info statusP    (progDesc "Show codchi status"))
    <> command "start"      (info startP     (progDesc "Start codchi controller"))
    <> command "install"    (info installP   (progDesc "Install a code machine"))
    <> command "uninstall"  (info uninstallP (progDesc "Uninstall a code machine"))
    <> command "run"        (info runP       (progDesc "Run command in a code machine"))
    <> command "update"     (info updateP    (progDesc "Update a code machine"))


    where
        statusP    = pure CommandStatus
        startP     = pure CommandStart
        uninstallP = CommandUninstall <$> argument parseable (metavar "NAME")
        runP       = CommandRun
                  <$> (not <$> switch (long "no-terminal" <> help "Whether to hide stdin / stdout"))
                  <*> argument parseable (metavar "NAME")
                  <*> many (argument str (metavar "CMD [ARGS...]"))
        updateP    = CommandUpdate    <$> argument parseable (metavar "NAME")

        installP = CommandInstall
                <$> argument parseable (metavar "NAME")
                <*> many (argument parseable (metavar "FLAKE_URL#MODULE_NAME"))


parseable :: Parseable p => ReadM p
parseable = eitherReader $ parseOnly parser . toText

parseCmd :: IO Command
parseCmd = execParser opts where
    opts = info (cmdP <**> helper)
         $ fullDesc
        <> header "CODe maCHInes - Declarative, Reproducible, Cross Platform Development Environments as Code"
