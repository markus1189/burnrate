-- | On-disk state, split by XDG semantics: API responses are refetchable and
--   live in the cache, the sampling log is not and lives in state.
{-# LANGUAGE LambdaCase #-}

module Burnrate.Store
  ( cacheDir
  , stateDir
  , readJSON
  , prune
  ) where

import           Data.Aeson (Value, decodeStrict)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe (fromMaybe)
import           System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import           System.Environment (lookupEnv)
import           System.FilePath ((</>))

cacheDir, stateDir :: IO FilePath
cacheDir = xdg "XDG_CACHE_HOME" ".cache"
stateDir = xdg "XDG_STATE_HOME" (".local" </> "state")

xdg :: String -> FilePath -> IO FilePath
xdg var fallback = do
  home <- getHomeDirectory
  root <- fromMaybe (home </> fallback) <$> lookupEnv var
  let d = root </> "burnrate"
  createDirectoryIfMissing True d >> pure d

-- | Strict read. A lazy ByteString would hold the handle open and the
--   subsequent write to the same path would fail with @resource busy@.
readJSON :: FilePath -> IO (Maybe Value)
readJSON f = doesFileExist f >>= \case
  False -> pure Nothing
  True  -> decodeStrict <$> BS.readFile f

-- | Bounded log: 60 days back, at most 400 points per key, oldest first.
prune :: Double -> [(Double, Double)] -> [(Double, Double)]
prune now xs = drop (length kept - 400) kept
  where kept = filter ((> now - 60 * 86400) . fst) xs
