{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_uber_proof (
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

bindir     = "/home/joeshanahan/projects/uber-proof/.stack-work/install/x86_64-linux/lts-9.2/8.0.2/bin"
libdir     = "/home/joeshanahan/projects/uber-proof/.stack-work/install/x86_64-linux/lts-9.2/8.0.2/lib/x86_64-linux-ghc-8.0.2/uber-proof-0.1.0.0-3gRQQgaKbokJNcLHSNxRVU"
dynlibdir  = "/home/joeshanahan/projects/uber-proof/.stack-work/install/x86_64-linux/lts-9.2/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/joeshanahan/projects/uber-proof/.stack-work/install/x86_64-linux/lts-9.2/8.0.2/share/x86_64-linux-ghc-8.0.2/uber-proof-0.1.0.0"
libexecdir = "/home/joeshanahan/projects/uber-proof/.stack-work/install/x86_64-linux/lts-9.2/8.0.2/libexec"
sysconfdir = "/home/joeshanahan/projects/uber-proof/.stack-work/install/x86_64-linux/lts-9.2/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "uber_proof_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "uber_proof_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "uber_proof_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "uber_proof_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "uber_proof_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "uber_proof_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
