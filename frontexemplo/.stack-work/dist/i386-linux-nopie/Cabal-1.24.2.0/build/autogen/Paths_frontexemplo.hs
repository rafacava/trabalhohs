{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_frontexemplo (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/rafael/trabalhohs/frontexemplo/.stack-work/install/i386-linux-nopie/lts-9.8/8.0.2/bin"
libdir     = "/home/rafael/trabalhohs/frontexemplo/.stack-work/install/i386-linux-nopie/lts-9.8/8.0.2/lib/i386-linux-ghc-8.0.2/frontexemplo-0.0.0-H0c8pw4HC1z92NDocJz3M4"
dynlibdir  = "/home/rafael/trabalhohs/frontexemplo/.stack-work/install/i386-linux-nopie/lts-9.8/8.0.2/lib/i386-linux-ghc-8.0.2"
datadir    = "/home/rafael/trabalhohs/frontexemplo/.stack-work/install/i386-linux-nopie/lts-9.8/8.0.2/share/i386-linux-ghc-8.0.2/frontexemplo-0.0.0"
libexecdir = "/home/rafael/trabalhohs/frontexemplo/.stack-work/install/i386-linux-nopie/lts-9.8/8.0.2/libexec"
sysconfdir = "/home/rafael/trabalhohs/frontexemplo/.stack-work/install/i386-linux-nopie/lts-9.8/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "frontexemplo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "frontexemplo_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "frontexemplo_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "frontexemplo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "frontexemplo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "frontexemplo_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
