module Paths_gameoflife (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/kalfonso/.cabal/bin"
libdir     = "/Users/kalfonso/.cabal/lib/gameoflife-0.0.1/ghc-7.0.3"
datadir    = "/Users/kalfonso/.cabal/share/gameoflife-0.0.1"
libexecdir = "/Users/kalfonso/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "gameoflife_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "gameoflife_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "gameoflife_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "gameoflife_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
