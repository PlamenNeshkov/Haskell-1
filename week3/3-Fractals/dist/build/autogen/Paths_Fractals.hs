module Paths_Fractals (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/plamen/Documents/Haskell-1/week3/3-Fractals/.cabal-sandbox/bin"
libdir     = "/home/plamen/Documents/Haskell-1/week3/3-Fractals/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/Fractals-0.1.0.0"
datadir    = "/home/plamen/Documents/Haskell-1/week3/3-Fractals/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/Fractals-0.1.0.0"
libexecdir = "/home/plamen/Documents/Haskell-1/week3/3-Fractals/.cabal-sandbox/libexec"
sysconfdir = "/home/plamen/Documents/Haskell-1/week3/3-Fractals/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Fractals_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Fractals_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Fractals_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Fractals_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Fractals_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
