-- | CLI: option parsing, the concurrent fetch, cache/sample bookkeeping, and
--   the choice of which row to print.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Burnrate.Estimate
import           Burnrate.Provider
import           Burnrate.Render
import           Burnrate.Spec
import           Burnrate.Store
import           Burnrate.Types
import           Control.Exception (SomeException, displayException, try)
import           Control.Lens ((^..), (^?))
import           Data.Aeson (Value (Object), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Lens (key, values)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List (nub, sortOn)
import           Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Prelude as Par
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.IO (hPutStrLn, stderr)
import           Text.Read (readMaybe)

data Opts = Opts
  { oAll      :: Bool
  , oTotal    :: Bool
  , oMaxAge   :: Double
  , oHalfLife :: Double
  , oStyle    :: String
  , oSpecs    :: [String]
  , oGroup    :: Maybe (String, Double)  -- ^ Display name, shared budget.
  , oPercent  :: Bool                    -- ^ Print only the projected percentage.
  }

defOpts :: Opts
defOpts = Opts False False 300 5 "none" [] Nothing False

parseOpts :: [String] -> Opts -> Opts
parseOpts [] o = o { oSpecs = reverse (oSpecs o) }
parseOpts (a:as) o = case (a, as) of
  ("--all", r)        -> parseOpts r o { oAll = True }
  ("--total", r)      -> parseOpts r o { oTotal = True }
  ("--percent", r)    -> parseOpts r o { oPercent = True }
  ("--group", v:r)    -> parseOpts r o { oGroup = grp v }
  ("--refresh", r)    -> parseOpts r o { oMaxAge = 0 }
  ("--max-age", v:r)  -> parseOpts r o { oMaxAge = rd v (oMaxAge o) }
  ("--halflife", v:r) -> parseOpts r o { oHalfLife = rd v (oHalfLife o) }
  ("--style", v:r)    -> parseOpts r o { oStyle = v }
  -- Unknown flags must not be swallowed as key specs: a typo'd --gruop would
  -- otherwise be sent to a provider as a password-store entry name.
  ('-':'-':_, _)      -> o { oSpecs = [] }
  _                   -> parseOpts as o { oSpecs = a : oSpecs o }
  where
    rd v d = fromMaybe d (readMaybe v)
    grp v = case break (== '=') v of  -- --group 150 | --group name=150
      (n, '=':l) -> (,) n <$> readMaybe l
      _          -> (,) "group" <$> readMaybe v

main :: IO ()
main = getArgs >>= \argv -> case parseOpts argv defOpts of
  o | null (oSpecs o) -> usage
    | otherwise -> do
      now   <- getCurrentTime
      epoch <- realToFrac <$> getPOSIXTime
      cdir  <- cacheDir
      sdir  <- stateDir
      specs <- concat <$> mapM (expand . parseSpec) (oSpecs o)
      sequence_ [hPutStrLn stderr "burnrate: no password-store entry matched" | null specs]
      let cfile = cdir </> "responses.json"
          sfile = sdir </> "samples.json"

      cached <- readJSON cfile
      let fresh = do
            c  <- cached
            ts <- c `at` "ts"
            if epoch - ts < oMaxAge o && (c ^? key "specs") == Just (toJSON (map spRaw specs))
              then Just c else Nothing

      raws <- case fresh of
        Just c -> pure [ maybe (Left (spRaw s <> ": not in cache")) Right
                               (c ^? key "raw" . key (K.fromString (spRaw s))) | s <- specs ]
        Nothing -> do
          rs <- Stream.fold Fold.toList
              . Par.parMapM (Par.maxThreads 8 . Par.ordered True) (fetchOne now)
              $ Stream.fromList specs
          BL.writeFile cfile . encode $ object
            [ "ts" .= epoch, "specs" .= map spRaw specs
            , "raw" .= object [K.fromString (spRaw s) .= v | (s, Right v) <- zip specs rs] ]
          pure rs

      mapM_ (hPutStrLn stderr . ("burnrate: " <>)) [e | Left e <- raws]
      -- OpenRouter labels unnamed keys with a masked form of the key itself,
      -- which tells a status bar nothing. Prefer the spec's basename.
      let relabel s acc
            | "sk-" `T.isPrefixOf` acLabel acc =
                acc { acLabel = T.pack (reverse (takeWhile (/= '/') (reverse (spRaw s)))) }
            | otherwise = acc
          accts = [(s, relabel s acc) | (s, Right v) <- zip specs raws, Just acc <- [parseAcct v]]

      old <- fromMaybe (object []) <$> readJSON sfile
      let stored s = [ (t, v) | p <- old ^.. key (K.fromString (spRaw s)) . values
                              , Just t <- [p `at` "t"], Just v <- [p `at` "v"] ]
          series (s, acc) = prune epoch (stored s ++ [(epoch, acSpent acc) | isNothing fresh])
      -- Left-biased union: keys absent from this run keep their history, which
      -- is the one thing here that cannot be refetched.
      BL.writeFile sfile . encode . Object $ KM.union
        (KM.fromList [ (K.fromString (spRaw s), toJSON [object ["t" .= t, "v" .= v]
                                                       | (t, v) <- series sa])
                     | sa@(s, _) <- accts ])
        (case old of { Object m -> m; _ -> KM.empty })

      let rows  = [(acc, estimate (oHalfLife o) now epoch (series sa) acc) | sa@(_, acc) <- accts]
          out r | oPercent o = putStrLn (paint (oStyle o) (rProjected r) (rPercent r))
                | otherwise  = putStrLn (paint (oStyle o) (rSeverity r) (rLine r))

          -- A provider-side group budget is shared across its member keys, so
          -- membership comes from the API's own group id rather than from spec
          -- names: a key added to or moved out of the group counts correctly
          -- with nothing to edit here.
          mem   = [x | x@(acc, _) <- rows, isJust (acGroup acc)]
          gids  = nub [g | (acc, _) <- mem, Just g <- [acGroup acc]]
          agg f = sum [f x | x <- mem]
          grpRow = do
            (gname, glim) <- oGroup o
            if length gids == 1 && not (null mem)
              then Just (render now (Just (T.pack gname)) (agg (acSpent . fst))
                           (Just (MonthlyLimit glim)) (agg (fst . snd))
                           (provisional [rp | (_, rp) <- mem]))
              else Nothing

          -- With a shared budget known, members' own limits are not the binding
          -- constraint and must not inflate the pool.
          lims = case (oGroup o, grpRow) of
            (Just (_, glim), Just _) ->
              glim : [l | (acc, _) <- rows, isNothing (acGroup acc), Just l <- [allowance acc]]
            _ -> [l | (acc, _) <- rows, Just l <- [allowance acc]]
          total = render now Nothing (sum [acSpent acc | (acc, _) <- rows])
                    (if null lims then Nothing else Just (MonthlyLimit (sum lims)))
                    (sum [r | (_, (r, _)) <- rows]) (provisional [rp | (_, rp) <- rows])
          reps  = sortOn rank $ maybeToList grpRow ++
                    [ render now (Just (acLabel acc)) (acSpent acc) (acBudget acc) r p
                    | (acc, (r, p)) <- rows ]
      sequence_ [ hPutStrLn stderr ("burnrate: --group ignored, keys span "
                                    <> show (length gids) <> " groups")
                | isJust (oGroup o), length gids > 1 ]
      case reps of
        [] -> hPutStrLn stderr "burnrate: no usable account" >> exitFailure
        rs@(worst:_) | oTotal o  -> out total
                     | oAll o    -> mapM_ out rs
                     | otherwise -> out worst
  where
    -- Soonest exhaustion first; never-exhausting rows sort last.
    rank r = (fromMaybe (1 / 0) (rWhen r), negate (rSeverity r))
    usage = mapM_ (hPutStrLn stderr)
      [ "usage: burnrate [--all|--total] [--percent] [--group [NAME=]LIMIT]"
      , "                [--refresh] [--max-age S] [--halflife D]"
      , "                [--style tmux|ansi|none] SPEC..."
      , ""
      , "--percent prints only the projected end-of-month spend as a percentage"
      , "of budget (spend so far plus the estimated rate over the days left)."
      , ""
      , "SPEC = [requesty:|openrouter:][pass:|env:|file:|cmd:]ARG"
      , "  pass specs may glob: '*'/'?' stop at '/', '**' crosses it (quote them)"
      , ""
      , "--group declares a shared provider-side budget (e.g. a Requesty group"
      , "budget, which the API will not disclose without manage permission)."
      , "Members are found via the group id the API reports for each key."
      , ""
      , "  burnrate api/requesty/main api/openrouter"
      , "  burnrate --all --style tmux 'pass:api/requesty/*'"
      , "  burnrate --group team=200 'pass:api/requesty/*'" ] >> exitFailure

fetchOne :: UTCTime -> Spec -> IO (Either String Value)
fetchOne now s = either (Left . trim . displayException) Right <$> attempt
  where
    attempt :: IO (Either SomeException Value)
    attempt = try $ do
      tok <- resolve (spSrc s)
      fetchRaw (fromMaybe (sniff tok) (spProv s)) tok (utctDay now)
    trim e = spRaw s <> ": " <> takeWhile (/= '\n') e
