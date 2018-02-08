{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_eta_fibers_dev (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/root/.etlas/bin"
libdir     = "/root/.etlas/lib/x86_64-linux-eta-0.7.0.1-ghc7_10_3/eta-fibers-dev-0.1.0.0-99dPUWzLxqHBnjG723XFqq"
dynlibdir  = "/root/.etlas/lib/x86_64-linux-eta-0.7.0.1-ghc7_10_3"
datadir    = "/root/.etlas/share/x86_64-linux-eta-0.7.0.1-ghc7_10_3/eta-fibers-dev-0.1.0.0"
libexecdir = "/root/.etlas/libexec"
sysconfdir = "/root/.etlas/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "eta_fibers_dev_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "eta_fibers_dev_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "eta_fibers_dev_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "eta_fibers_dev_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "eta_fibers_dev_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "eta_fibers_dev_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
