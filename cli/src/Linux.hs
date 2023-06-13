module Linux where


import Cleff
import Devenv
import Util
import Types

runDevevIO :: Interpreter Devenv (Logger : IOE : Errors [DriverException, UserError, Panic])
runDevevIO = interpretIO $ \case
    _ -> error "not implemented"
