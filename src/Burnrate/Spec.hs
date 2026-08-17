{-# LANGUAGE OverloadedStrings #-}

-- | Key specs: parsing, provider sniffing, and glob expansion over @pass@.
module Burnrate.Spec
  ( parseSpec,
    resolve,
    sniff,
    expand,
    storeDir,
    walk,
    glob,
    passEntries,
    envEntries,
    fileEntries,
  )
where

import Burnrate.Types
import Control.Monad (forM)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.FilePath (addTrailingPathSeparator, (</>))
import System.Process (readCreateProcess, readProcess, shell)

-- | @[provider:]source:arg@. An unrecognised prefix is not an error: it is
--   treated as a @pass@ entry name, so the common case stays terse.
parseSpec :: String -> Spec
parseSpec raw = Spec prov (src rest) raw
  where
    (prov, rest) = case split1 raw of
      Just ("requesty", r) -> (Just Requesty, r)
      Just ("openrouter", r) -> (Just OpenRouter, r)
      _ -> (Nothing, raw)
    src s = case split1 s of
      Just ("pass", r) -> Pass r
      Just ("env", r) -> Env r
      Just ("file", r) -> File r
      Just ("cmd", r) -> Cmd r
      _ -> Pass s
    split1 s = case break (== ':') s of (x, ':' : y) -> Just (x, y); _ -> Nothing

-- | Only the first line is taken: @pass@ entries conventionally carry metadata
--   on subsequent lines.
resolve :: Src -> IO Text
resolve s =
  T.strip . T.pack . takeWhile (/= '\n') <$> case s of
    Pass e -> readProcess "pass" [e] ""
    File f -> readFile f
    Cmd c -> readCreateProcess (shell c) ""
    Env v -> lookupEnv v >>= maybe (fail ("unset env var " <> v)) pure

-- | Key formats are self-identifying, so provider selection needs no config.
sniff :: Text -> Provider
sniff t = if "sk-or-" `T.isPrefixOf` t then OpenRouter else Requesty

-- | Expand a @pass@ glob against the password store, so one spec can name a
--   whole subtree. Expansion yields concrete entry names, which keeps the
--   sampling log keyed per key rather than per glob — a glob that later matches
--   more keys must not orphan the history of the ones it already matched.
expand :: Spec -> IO [Spec]
expand (Spec p (Pass e) _)
  | any (`elem` ("*?" :: String)) e = do
      root <- storeDir
      ok <- doesDirectoryExist root
      ms <- if ok then sort . filter (glob e) <$> walk root "" else pure []
      pure [Spec p (Pass m) m | m <- ms]
expand s = pure [s]

storeDir :: IO FilePath
storeDir = do
  home <- getHomeDirectory
  fromMaybe (home </> ".password-store") <$> lookupEnv "PASSWORD_STORE_DIR"

-- | Every @pass@ entry under the store, relative and without the @.gpg@
--   suffix, in no particular order. No globbing here: the caller (the
--   completion engine) is responsible for matching, so a @\"pass:api/requesty/*\"@
--   prefix returns the whole subtree for it to filter.
passEntries :: IO [FilePath]
passEntries = do
  root <- storeDir
  ok <- doesDirectoryExist root
  if ok then walk root "" else pure []

-- | The names of the currently-set environment variables, since @env:@ specs
--   name an environment variable. Underscore-prefixed names (@_@, @_GCC@…) are
--   shell shadowing, never something you'd point a key at, so drop them.
envEntries :: IO [String]
envEntries = filter (not . isPrefixOf "_") . map fst <$> getEnvironment

-- | Entries in a directory, each trailing @\/@ when it names a subdirectory,
--   so a @file:@ spec can keep typing down into a tree. Paths are absolute
--   (the @file:@ source in 'parseSpec' is a verbatim path), unlike @pass@
--   entries which are relative to the store.
fileEntries :: FilePath -> IO [FilePath]
fileEntries dir = do
  ok <- doesDirectoryExist dir
  if not ok
    then pure []
    else do
      ns <- listDirectory dir
      fmap concat . forM (filter (not . isPrefixOf ".") ns) $ \n -> do
        let r = dir </> n
        isDir <- doesDirectoryExist r
        pure [if isDir then addTrailingPathSeparator r else r]

-- | Password-store entries, relative and without the @.gpg@ suffix.
walk :: FilePath -> FilePath -> IO [FilePath]
walk root rel = do
  ns <- listDirectory (root </> rel)
  fmap concat . forM (filter (not . isPrefixOf ".") ns) $ \n -> do
    let r = if null rel then n else rel </> n
    isDir <- doesDirectoryExist (root </> r)
    if isDir
      then walk root r
      else pure [take (length r - 4) r | ".gpg" `isSuffixOf` r]

-- | @*@ and @?@ stop at @\/@; @**@ crosses it.
glob :: String -> String -> Bool
glob ('*' : '*' : p) s = glob p s || (not (null s) && glob ('*' : '*' : p) (drop 1 s))
glob ('*' : p) s = glob p s || case s of c : cs | c /= '/' -> glob ('*' : p) cs; _ -> False
glob ('?' : p) (c : cs) | c /= '/' = glob p cs
glob (c : p) (d : cs) | c == d = glob p cs
glob [] [] = True
glob _ _ = False
