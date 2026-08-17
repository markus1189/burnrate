{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CLI: option parsing, the concurrent fetch, cache/sample bookkeeping, and
--   the choice of which row to print.
--
--   Argument parsing is optparse-applicative, driven by a single 'Parser'
--   declaration. The same declaration runs the real parser, tab-completes SPEC
--   arguments, and (via the generated script flags) produces the shipped
--   bash/zsh/fish completion files.
module Main (main) where

import Burnrate.Completion (specCompleter)
import Burnrate.Estimate
import Burnrate.Provider
import Burnrate.Render
import Burnrate.Spec
import Burnrate.Store
import Burnrate.Types
import Control.Exception (SomeException, displayException, try)
import Control.Lens ((^..), (^?))
import Data.Aeson (Value (Object), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, values)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import Options.Applicative
import Options.Applicative.BashCompletion
  ( bashCompletionScript,
    fishCompletionScript,
    zshCompletionScript,
  )
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Prelude as Par
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Text.Read (readEither)

data Opts = Opts
  { oAll :: Bool,
    oTotal :: Bool,
    oPercent :: Bool,
    oRefresh :: Bool,
    oMaxAge :: Double,
    oHalfLife :: Double,
    oStyle :: String,
    -- | Display name, shared budget.
    oGroup :: Maybe (String, Double),
    oSpecs :: [String]
  }

-- | --refresh is a semantic alias for --max-age 0, kept as its own flag so it
--   composes with the option instead of fighting over a shared value with it.
refreshAge :: Opts -> Double
refreshAge o = if oRefresh o then 0 else oMaxAge o

opts :: Parser Opts
opts =
  Opts
    <$> switch (long "all" <> help "one line per key, soonest exhaustion first")
    <*> switch (long "total" <> help "one aggregated line, no label")
    <*> switch (long "percent" <> help "print only the projected end-of-month percentage")
    <*> switch (long "refresh" <> help "ignore the response cache and refetch (== --max-age 0)")
    <*> option
      auto
      ( long "max-age"
          <> metavar "SECONDS"
          <> value 300
          <> showDefault
          <> help "use cached responses younger than this"
      )
    <*> option auto (long "halflife" <> metavar "DAYS" <> value 5 <> showDefault)
    <*> strOption (long "style" <> value "none" <> metavar "tmux|ansi|none")
    <*> optional
      ( option
          (eitherReader parseGroup)
          (long "group" <> metavar "[NAME=]LIM")
      )
    <*> some (argument str (metavar "SPEC…" <> completer specCompleter))

-- | @--group 150@ names the group \"group\"; @--group team=150@ names it
--   \"team\". A non-numeric limit is a genuine parse error, not a silent
--   fallback to a group of zero (which used to hide typos).
parseGroup :: String -> Either String (String, Double)
parseGroup v = case break (== '=') v of
  (n, '=' : l) -> (,) n <$> readDouble l
  _ -> (,) "group" <$> readDouble v
  where
    readDouble :: String -> Either String Double
    readDouble = readEither

infoDesc :: ParserInfo Opts
infoDesc =
  info (opts <**> helper) $
    fullDesc
      <> header "burnrate — LLM spend & projected burn, for a status bar"
      <> progDesc
        ( "SPEC = [requesty:|openrouter:][pass:|env:|file:|cmd:]ARG — "
            ++ "pass specs may glob a password-store subtree; a bare ARG is "
            ++ "itself a password-store entry."
        )

-- | The generated completion scripts re-invoke the binary as `burnrate`, so the
--   program name the scripts embed is fixed (not the name the user aliased).
progName :: String
progName = "burnrate"

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ("--zsh-completion-script" : _) -> putStr (zshCompletionScript progName progName)
    ("--bash-completion-script" : _) -> putStr (bashCompletionScript progName progName)
    ("--fish-completion-script" : _) -> putStr (fishCompletionScript progName progName)
    _ -> execParser infoDesc >>= runOpts

-- | The sampling/cache/print pipeline, now keyed off a parsed 'Opts'. Unchanged
--   in behaviour from the hand-rolled parser except that a parse error (bad
--   flag, @--group foo=banana@, no SPECs) exits 1 from 'execParser' before this
--   is reached.
runOpts :: Opts -> IO ()
runOpts o = do
  now <- getCurrentTime
  epoch <- realToFrac <$> getPOSIXTime
  cdir <- cacheDir
  sdir <- stateDir
  specs <- concat <$> mapM (expand . parseSpec) (oSpecs o)
  sequence_ [hPutStrLn stderr "burnrate: no password-store entry matched" | null specs]
  let cfile = cdir </> "responses.json"
      sfile = sdir </> "samples.json"

  cached <- readJSON cfile
  let fresh = do
        c <- cached
        ts <- c `at` "ts"
        if epoch - ts < refreshAge o && (c ^? key "specs") == Just (toJSON (map spRaw specs))
          then Just c
          else Nothing

  raws <- case fresh of
    Just c ->
      pure
        [ maybe
            (Left (spRaw s <> ": not in cache"))
            Right
            (c ^? key "raw" . key (K.fromString (spRaw s)))
        | s <- specs
        ]
    Nothing -> do
      rs <-
        Stream.fold Fold.toList
          . Par.parMapM (Par.maxThreads 8 . Par.ordered True) (fetchOne now)
          $ Stream.fromList specs
      BL.writeFile cfile . encode $
        object
          [ "ts" .= epoch,
            "specs" .= map spRaw specs,
            "raw" .= object [K.fromString (spRaw s) .= v | (s, Right v) <- zip specs rs]
          ]
      pure rs

  mapM_ (hPutStrLn stderr . ("burnrate: " <>)) [e | Left e <- raws]
  -- OpenRouter labels unnamed keys with a masked form of the key itself,
  -- which tells a status bar nothing. Prefer the spec's basename.
  let relabel s acc
        | "sk-" `T.isPrefixOf` acLabel acc =
            acc {acLabel = T.pack (reverse (takeWhile (/= '/') (reverse (spRaw s))))}
        | otherwise = acc
      accts = [(s, relabel s acc) | (s, Right v) <- zip specs raws, Just acc <- [parseAcct v]]

  old <- fromMaybe (object []) <$> readJSON sfile
  let stored s =
        [ (t, v)
        | p <- old ^.. key (K.fromString (spRaw s)) . values,
          Just t <- [p `at` "t"],
          Just v <- [p `at` "v"]
        ]
      series (s, acc) = prune epoch (stored s ++ [(epoch, acSpent acc) | isNothing fresh])
  -- Left-biased union: keys absent from this run keep their history, which
  -- is the one thing here that cannot be refetched.
  BL.writeFile sfile . encode . Object $
    KM.union
      ( KM.fromList
          [ ( K.fromString (spRaw s),
              toJSON
                [ object ["t" .= t, "v" .= v]
                | (t, v) <- series sa
                ]
            )
          | sa@(s, _) <- accts
          ]
      )
      (case old of Object m -> m; _ -> KM.empty)

  let rows = [(acc, estimate (oHalfLife o) now epoch (series sa) acc) | sa@(_, acc) <- accts]
      out r
        | oPercent o = putStrLn (paint (oStyle o) (rProjected r) (rPercent r))
        | otherwise = putStrLn (paint (oStyle o) (rSeverity r) (rLine r))

      -- A provider-side group budget is shared across its member keys, so
      -- membership comes from the API's own group id rather than from spec
      -- names: a key added to or moved out of the group counts correctly
      -- with nothing to edit here.
      mem = [x | x@(acc, _) <- rows, isJust (acGroup acc)]
      gids = nub [g | (acc, _) <- mem, Just g <- [acGroup acc]]
      agg f = sum [f x | x <- mem]
      -- Not the declared budget but what these keys can reach: pass a
      -- subset of the group and their own limits, not the shared one, are
      -- what they will hit first.
      pool = flip groupPool [acc | (acc, _) <- mem]
      grpRow = do
        (gname, glim) <- oGroup o
        if length gids == 1 && not (null mem)
          then
            Just
              ( render
                  now
                  (Just (T.pack gname))
                  (agg (acSpent . fst))
                  (Just (MonthlyLimit (pool glim)))
                  (agg (fst . snd))
                  (provisional [rp | (_, rp) <- mem])
              )
          else Nothing

      -- With a shared budget known, members' own limits are folded into the
      -- pool by 'groupPool' and must not be added a second time.
      lims = case (oGroup o, grpRow) of
        (Just (_, glim), Just _) ->
          pool glim
            : [l | (acc, _) <- rows, isNothing (acGroup acc), Just l <- [allowance acc]]
        _ -> [l | (acc, _) <- rows, Just l <- [allowance acc]]
      total =
        render
          now
          Nothing
          (sum [acSpent acc | (acc, _) <- rows])
          (if null lims then Nothing else Just (MonthlyLimit (sum lims)))
          (sum [r | (_, (r, _)) <- rows])
          (provisional [rp | (_, rp) <- rows])
      reps =
        sortOn rank $
          maybeToList grpRow
            ++ [ render now (Just (acLabel acc)) (acSpent acc) (acBudget acc) r p
               | (acc, (r, p)) <- rows
               ]
  sequence_
    [ hPutStrLn
        stderr
        ( "burnrate: --group ignored, keys span "
            <> show (length gids)
            <> " groups"
        )
    | isJust (oGroup o),
      length gids > 1
    ]
  case reps of
    [] -> hPutStrLn stderr "burnrate: no usable account" >> exitFailure
    rs@(worst : _)
      | oTotal o -> out total
      | oAll o -> mapM_ out rs
      | otherwise -> out worst
  where
    -- Soonest exhaustion first; never-exhausting rows sort last.
    rank r = (fromMaybe (1 / 0) (rWhen r), negate (rSeverity r))

fetchOne :: UTCTime -> Spec -> IO (Either String Value)
fetchOne now s = either (Left . trim . displayException) Right <$> attempt
  where
    attempt :: IO (Either SomeException Value)
    attempt = try $ do
      tok <- resolve (spSrc s)
      fetchRaw (fromMaybe (sniff tok) (spProv s)) tok (utctDay now)
    trim e = spRaw s <> ": " <> takeWhile (/= '\n') e
