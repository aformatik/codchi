module Parser where

import Data.Attoparsec.Text (parseOnly)
import Options.Applicative

import Types
import Util                 (Parseable, parser)

data Command
    = CommandStatus
    | CommandStart
    | CommandInstall InstanceName [DevenvFlake ()]
    | CommandUninstall InstanceName
    | CommandRun InstanceName
    | CommandUpdate InstanceName
  deriving (Eq, Show)

cmdP :: Parser Command
cmdP = subparser
     $ command "status"     (info statusP    (progDesc "Show devenv status"))
    <> command "start"      (info startP     (progDesc "Start devenv controller"))
    <> command "install"    (info installP   (progDesc "Install devenv instance"))
    <> command "uninstall"  (info uninstallP (progDesc "Uninstall devenv instance"))
    <> command "run"        (info runP       (progDesc "Run devenv instance"))
    <> command "update"     (info updateP    (progDesc "Update devenv instance"))


    where
        statusP    = pure CommandStatus
        startP     = pure CommandStart
        uninstallP = CommandUninstall <$> argument parseable (metavar "NAME")
        runP       = CommandRun       <$> argument parseable (metavar "NAME")
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
        <> header "devenv - cross platform development environment from code"
