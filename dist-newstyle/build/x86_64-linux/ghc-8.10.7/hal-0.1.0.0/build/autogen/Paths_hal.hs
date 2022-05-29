{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_hal (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/creep/.cabal/bin"
libdir     = "/home/creep/.cabal/lib/x86_64-linux-ghc-8.10.7/hal-0.1.0.0-inplace"
dynlibdir  = "/home/creep/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/creep/.cabal/share/x86_64-linux-ghc-8.10.7/hal-0.1.0.0"
libexecdir = "/home/creep/.cabal/libexec/x86_64-linux-ghc-8.10.7/hal-0.1.0.0"
sysconfdir = "/home/creep/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hal_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hal_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
