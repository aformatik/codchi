{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codchi.Platform.Windows.Internal where

#ifdef mingw32_HOST_OS
import Graphics.Win32 hiding (try)
import System.Win32.Console.CtrlHandler
import System.Win32.DLL (getModuleHandle)
import System.Win32.Registry
import System.Win32.Shell
#else
import Foreign (ForeignPtr, Ptr, nullPtr)
import Foreign.C (CInt, CIntPtr, CWchar)
#endif

import Codchi.Types

import Codchi.Config
import Control.Exception (bracket)
import qualified Data.Text as Text

-- https://tarma.com/support/im9/using/symbols/functions/csidls.htm
cSIDL_PROGRAMS :: CSIDL
cSIDL_PROGRAMS = 2
cSIDL_PROGRAM_FILESx86 :: CSIDL
cSIDL_PROGRAM_FILESx86 = 42

getFolderPath :: MonadIO m => CSIDL -> m (Path Abs)
getFolderPath csidl = mkNTPath <$> liftIO (sHGetFolderPath nullPtr csidl nullPtr 0)

mkNTPath :: ToText txt => txt -> Path t
mkNTPath = Path . filter (not . Text.null) . Text.splitOn "\\" . toText

toNTPath :: Path t -> Text
toNTPath (Path p) = Text.intercalate "\\" p

-- https://otter-o.hatenadiary.org/entry/20090217/1234861028
runWinLoop :: MonadIO m => IO () -> m ()
runWinLoop cleanup = liftIO $ do
    withConsoleCtrlHandler (\_ -> cleanup >> exitSuccess >> pure True) $ do
        void $ createMessageWindow msgLoop
        allocaMessage messagePump
  where
    createMessageWindow wndProc = do
        let clsName = mkClassName _APP_NAME
        hinst <- getModuleHandle Nothing
        whenNothingM_ (registerClass (0, hinst, Nothing, Nothing, Nothing, Nothing, clsName)) $
            error "Couldn't register window class"
        createWindow
            clsName
            _APP_NAME
            0
            Nothing
            Nothing
            Nothing
            Nothing
            (Just nullPtr)
            Nothing
            hinst
            wndProc
    msgLoop hwnd msg wp lp
        | msg == wM_CLOSE = cleanup >> exitSuccess >> return 0
        | otherwise = liftIO $ defWindowProc (Just hwnd) msg wp lp
    messagePump msg = void $ infinitely $ do
        whenM (getMessage msg Nothing) $ do
            void $ translateMessage msg
            void $ dispatchMessage msg

findWslVmId :: IO (Maybe String)
findWslVmId = do
    let vmsKey = regOpenKeyEx hKEY_LOCAL_MACHINE "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\HostComputeService\\VolatileStore\\ComputeSystem" kEY_READ
    withRegKey vmsKey $ \hkey -> do
        vmKeys <- regEnumKeys hkey

        let filterWslVms vmid = do
                subkeys <- withRegKey (regOpenKeyEx hkey vmid kEY_READ) regEnumKeyVals

                return $
                    if any isWslVm subkeys
                        then Just vmid
                        else Nothing

        asum <$> mapM filterWslVms vmKeys
  where
    isWslVm ("ComputeSystemType", "2", _) = True
    isWslVm _ = False

withRegKey :: IO HKEY -> (HKEY -> IO c) -> IO c
withRegKey aquire = bracket aquire regCloseKey

#ifdef mingw32_HOST_OS
foreign import ccall "shlobj_core.h SHChangeNotify"
    c_SHChangeNotify :: LONG -> UINT32 -> LPVOID -> LPVOID -> IO ()

refreshIconCache :: IO ()
refreshIconCache = c_SHChangeNotify 0x8000000 0x1000 nullPtr nullPtr

-- foreign import ccall "windows.h FreeConsole"
--     c_FreeConsole :: IO BOOL

-- freeConsole :: IO ()
-- freeConsole = failIfFalse_ "FreeConsole" c_FreeConsole

foreign import ccall "windows.h GetConsoleWindow"
    c_GetConsoleWindow :: IO HWND

getConsoleWindow :: IO (Maybe HWND)
getConsoleWindow = do
    hwnd <- c_GetConsoleWindow
    return $ if hwnd /= nullPtr
        then Just hwnd
        else Nothing

#else

-- fake functions for HLS under linux

noop :: a
noop = error "Not implemented on linux"

type DWORD = Word32
type WindowStyle = DWORD
type LONG = Int32
type LPMSG = Ptr ()
type HWND = Ptr ()
type HMENU = Ptr ()
type HANDLE = Ptr ()
type HINSTANCE = Ptr ()
type LPTSTR = Ptr CWchar
type WindowClosure = HWND -> DWORD -> Word -> CIntPtr -> IO CIntPtr
type REGSAM = Word32
type HKEY = ForeignPtr ()
type CSIDL = CInt

wM_CLOSE :: DWORD
wM_CLOSE = noop
-- wM_DESTROY :: DWORD
-- wM_DESTROY = noop

failIfFalse_ :: String -> IO Bool -> IO ()
failIfFalse_ = noop

withConsoleCtrlHandler :: (DWORD -> IO Bool) -> IO a -> IO a
withConsoleCtrlHandler = noop

getMessage :: LPMSG -> Maybe HWND -> IO Bool
getMessage = noop
allocaMessage :: (LPMSG -> IO a) -> IO a
allocaMessage = noop
translateMessage :: LPMSG -> IO Bool
translateMessage = noop
dispatchMessage :: LPMSG -> IO LONG
dispatchMessage = noop
defWindowProc :: Maybe HWND -> DWORD -> Word -> CIntPtr -> IO CIntPtr
defWindowProc = noop
-- postQuitMessage :: Int -> IO ()
-- postQuitMessage = noop

mkClassName :: String -> a
mkClassName = noop
registerClass :: (Word32, HINSTANCE, Maybe HANDLE, Maybe HANDLE, Maybe HANDLE, Maybe LPTSTR, LPTSTR) -> IO (Maybe Word16)
registerClass = noop
createWindow
  :: LPTSTR -> String -> WindowStyle ->
     Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int ->
     Maybe HWND -> Maybe HMENU -> HINSTANCE -> WindowClosure ->
     IO HWND
createWindow = noop

type ShowWindowControl = DWORD
sW_HIDE :: ShowWindowControl
sW_HIDE = noop
showWindow :: HWND -> ShowWindowControl -> IO Bool
showWindow = noop

getModuleHandle :: Maybe String -> IO HINSTANCE
getModuleHandle = noop

hKEY_LOCAL_MACHINE :: HKEY
hKEY_LOCAL_MACHINE = noop
kEY_READ :: REGSAM
kEY_READ = noop
regOpenKeyEx :: HKEY -> String -> REGSAM -> IO HKEY
regOpenKeyEx = noop
regCloseKey :: HKEY -> IO ()
regCloseKey = noop
regEnumKeys :: HKEY -> IO [String]
regEnumKeys = noop
regEnumKeyVals :: HKEY -> IO [(String,String,DWORD)]
regEnumKeyVals = noop

cSIDL_PROGRAM_FILES :: CSIDL
cSIDL_PROGRAM_FILES = 38
type SHGetFolderPathFlags = DWORD
sHGetFolderPath :: HWND -> CSIDL -> HANDLE -> SHGetFolderPathFlags -> IO String
sHGetFolderPath = noop

-- win32-shortcut

data ShowCmd = ShowNormal | ShowMaximized | ShowMinimized deriving (Show)
data Shortcut = Shortcut
    { targetPath       :: FilePath
    , arguments        :: String
    , workingDirectory :: FilePath
    , showCmd          :: ShowCmd
    , description      :: String
    , iconLocation     :: (FilePath, Int)
    , hotkey           :: DWORD
    }
  deriving (Show)

type ShortcutError = Void
initialize :: IO (Either ShortcutError ())
initialize = noop
uninitialize :: IO ()
uninitialize = noop
writeShortcut :: Shortcut -> FilePath -> IO (Either ShortcutError ())
writeShortcut = noop
refreshIconCache :: IO ()
refreshIconCache = noop

-- freeConsole :: IO ()
-- freeConsole = noop
getConsoleWindow :: IO (Maybe HWND)
getConsoleWindow = noop
#endif
