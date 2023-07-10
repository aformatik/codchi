module Linux where


import Cleff
import Dsl
import Util
import Types

runCodchiLIO :: Interpreter CodchiL (Logger : IOE : Errors [DriverException, UserError, Panic])
runCodchiLIO = interpretIO $ \case
    _ -> return undefined -- error "not implemented"
