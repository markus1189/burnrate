{-# LANGUAGE OverloadedStrings #-}

-- | Pins the behaviour of every pure function, and in particular the four bugs
--   that shipped looking healthy during development: an empty history reporting
--   a confident rate of zero, idle samples implying "never exhausts", sparse
--   day maps inflating the average, and a provisional marker that always fired.
module Main (main) where

import Burnrate.Completion (completeSpec)
import Burnrate.Estimate
import Burnrate.Provider (parseAcct)
import Burnrate.Render
import Burnrate.Spec (glob, parseSpec)
import Burnrate.Store (prune)
import Burnrate.Types
import Data.Aeson (Value, object, (.=))
import Data.List (isPrefixOf, sort)
import qualified Data.Map.Strict as M
import Data.Time
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Environment (setEnv)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec

now :: UTCTime
now = UTCTime (fromGregorian 2026 8 14) (12 * 3600) -- half way through Aug 14

near :: Double -> Double -> Expectation
near a b = abs (a - b) `shouldSatisfy` (< 1e-9)

acct :: Account
acct = Account "k" 0 Nothing M.empty Nothing

lim :: Double -> Account
lim l = acct {acBudget = Just (MonthlyLimit l)}

-- | A throwaway PASSWORD_STORE layout with a couple of entries, the same ones
--   the `pass:` completion tests expect. `.gpg` suffixes are the only content
--   that matters, so an empty file suffices.
mkFakeStore :: IO FilePath
mkFakeStore = do
  dir <- getTemporaryDirectory >>= \t -> pure (t </> "burnrate-test-store")
  let paths =
        [ dir </> "api/requesty/main.gpg",
          dir </> "api/requesty/sub/agent.gpg",
          dir </> "api/openrouter/key.gpg"
        ]
  mapM_ (createDirectoryIfMissing True . takeDirectory) paths
  mapM_ (\p -> writeFile p "") paths
  pure dir

main :: IO ()
main = hspec $ do
  describe "glob" $ do
    it "does not let * cross a separator" $ do
      glob "api/requesty/*" "api/requesty/agent" `shouldBe` True
      glob "api/requesty/*" "api/requesty/sub/agent" `shouldBe` False
    it "lets ** cross separators" $ do
      glob "api/**" "api/requesty/agent" `shouldBe` True
      glob "api/**" "api" `shouldBe` False
    it "matches ? against exactly one non-separator" $ do
      glob "ke?" "key" `shouldBe` True
      glob "ke?" "keys" `shouldBe` False
    it "matches literals exactly" $ do
      glob "api/openrouter" "api/openrouter" `shouldBe` True
      glob "api/openrouter" "api/openrouter2" `shouldBe` False

  describe "parseSpec" $ do
    it "defaults a bare word to a pass entry with sniffed provider" $ do
      let s = parseSpec "api/requesty/agent"
      (spProv s, spSrc s) `shouldBe` (Nothing, Pass "api/requesty/agent")
    it "reads an explicit provider and source" $ do
      let s = parseSpec "openrouter:env:OR_KEY"
      (spProv s, spSrc s) `shouldBe` (Just OpenRouter, Env "OR_KEY")
    it "keeps the raw string for cache and sample keying" $
      spRaw (parseSpec "pass:api/x") `shouldBe` "pass:api/x"
    it "treats an unknown prefix as part of a pass entry" $
      spSrc (parseSpec "weird:thing") `shouldBe` Pass "weird:thing"

  describe "rate" $ do
    it "returns the common rate when every interval agrees" $
      rate 5 [(1, 10, 1), (2, 10, 1)] `shouldSatisfy` maybe False (\r -> abs (r - 10) < 1e-9)
    it "weights by elapsed time, so idle polls cannot outvote a busy interval" $ do
      -- one busy day, then two 1-minute idle samples: still ~10/day, not ~0
      let m = 1 / 1440
      case rate 5 [(1, 10, 1), (0, 0, m), (0, 0, m)] of
        Just r -> r `shouldSatisfy` (> 9)
        Nothing -> expectationFailure "expected a rate"
    it "is Nothing when no time has elapsed" $
      rate 5 [] `shouldBe` Nothing

  describe "histIntervals" $ do
    it "zero-fills days the provider omitted" $ do
      let today = fromGregorian 2026 8 5
          h = M.fromList [(fromGregorian 2026 8 3, 5)]
          ivs = histIntervals today h
      length ivs `shouldBe` 4 -- Aug 1..4, today excluded
      sum [s | (_, s, _) <- ivs] `near` 5
      sum [t | (_, _, t) <- ivs] `near` 4 -- four whole days, not one
    it "excludes today, which is only partly elapsed" $
      histIntervals (fromGregorian 2026 8 1) M.empty `shouldBe` []

  describe "sampleIntervals" $ do
    it "differences consecutive samples" $
      sampleIntervals 172800 [(86400, 10), (172800, 25)] `shouldBe` [(0, 15, 1)]
    it "drops non-monotonic pairs, which are month rollovers" $
      sampleIntervals 172800 [(86400, 30), (172800, 2)] `shouldBe` []

  describe "estimate" $ do
    it "does not report a confident zero for an empty history" $ do
      -- The bug: histIntervals yields one interval per calendar day regardless
      -- of data, so an empty map used to produce Just 0 and "never exhausts".
      let a = acct {acSpent = 40}
      snd (estimate 5 now 0 [] a) `shouldBe` True -- provisional, not sure
      fst (estimate 5 now 0 [] a) `shouldSatisfy` (> 0)
    it "ignores a sampling log spanning less than six hours" $ do
      let a = acct {acSpent = 40}
          ss = [(0, 40), (600, 40)] -- ten minutes, idle
      snd (estimate 5 now 600 ss a) `shouldBe` True -- fell back, not 0/day
    it "prefers real daily history and marks it non-provisional" $ do
      let a =
            acct
              { acSpent = 40,
                acHist = M.fromList [(fromGregorian 2026 8 d, 4) | d <- [1 .. 13]]
              }
      snd (estimate 5 now 0 [] a) `shouldBe` False
      fst (estimate 5 now 0 [] a) `near` 4

  describe "provisional" $ do
    it "ignores guessed rows that contribute nothing" $
      provisional [(10, False), (0, True)] `shouldBe` False
    it "fires when guesses carry a material share" $
      provisional [(2, True), (8, False)] `shouldBe` True
    it "is False for an all-zero aggregate" $
      provisional [(0, True), (0, True)] `shouldBe` False

  describe "allowance" $ do
    it "passes a monthly limit through" $
      allowance acct {acBudget = Just (MonthlyLimit 150)} `shouldBe` Just 150
    it "converts a prepaid balance to spent-plus-remaining" $
      allowance acct {acSpent = 12, acBudget = Just (Remaining 5)} `shouldBe` Just 17
    it "is Nothing when unmetered" $
      allowance acct `shouldBe` Nothing

  describe "groupPool" $ do
    it "uses the shared budget when the members can actually reach it" $
      groupPool 150 [lim 150, lim 25, lim 15, lim 5] `near` 150
    it "falls back to the members' own limits when they cannot" $
      -- The bug: one key of a 150 group, capped at 15 itself, was reported as
      -- 5% of 150 rather than 46% of the 15 it will actually hit.
      groupPool 150 [lim 15] `near` 15
    it "counts a prepaid balance as spent-plus-remaining" $
      groupPool 150 [acct {acSpent = 12, acBudget = Just (Remaining 5)}] `near` 17
    it "is the shared budget alone when any member is unmetered" $
      groupPool 150 [lim 15, acct] `near` 150
    it "is the shared budget for no members at all" $
      groupPool 150 [] `near` 150

  describe "prune" $ do
    it "drops points older than sixty days" $
      prune 0 [(-61 * 86400, 1), (-1, 2)] `shouldBe` [(-1, 2)]
    it "keeps at most four hundred points, newest last" $ do
      let xs = [(fromIntegral i, fromIntegral i) | i <- [1 .. 500 :: Int]]
      length (prune 0 xs) `shouldBe` 400
      last (prune 0 xs) `shouldBe` (500, 500)

  describe "render" $ do
    it "omits the label for an aggregate row" $
      rLine (render now Nothing 83 (Just (MonthlyLimit 150)) 5 False)
        `shouldBe` "$83/$150 1.3x → Aug 27"
    it "prefixes the label otherwise" $
      rLine (render now (Just "agent") 83 (Just (MonthlyLimit 150)) 5 False)
        `shouldBe` "agent $83/$150 1.3x → Aug 27"
    it "omits the arrow when the month resets first" $
      rLine (render now Nothing 83 (Just (MonthlyLimit 150)) 1 False)
        `shouldBe` "$83/$150 1.3x"
    it "marks a provisional estimate with a tilde" $
      rLine (render now Nothing 83 (Just (MonthlyLimit 150)) 1 True)
        `shouldBe` "$83/$150 ~1.3x"
    it "reports runway, not pace, for a prepaid balance" $
      rLine (render now (Just "t") 1 (Just (Remaining 6.42)) 0.5 False)
        `shouldSatisfy` isPrefixOf "t $6.42 left → "
    it "shows spend alone when unmetered" $
      rLine (render now (Just "x") 5 Nothing 1 False) `shouldBe` "x $5.00 mtd"
    it "escalates severity with pace" $
      rSeverity (render now Nothing 83 (Just (MonthlyLimit 150)) 5 False)
        `shouldSatisfy` (> 1.25)

  describe "render --percent" $ do
    it "projects to the end of the month, not just where spend stands now" $
      -- \$83 now + $5/day over the remaining 17.5 days = $170.50 of $150
      rPercent (render now Nothing 83 (Just (MonthlyLimit 150)) 5 False) `shouldBe` "114%"
    it "differs from pace, which only annualises the average so far" $ do
      let rep = render now Nothing 83 (Just (MonthlyLimit 150)) 5 False
      rProjected rep `near` (170.5 / 150)
      abs (rProjected rep - rSeverity rep) `shouldSatisfy` (> 0.01)
    it "keeps the label under --all and drops it for an aggregate" $ do
      rPercent (render now (Just "agent") 83 (Just (MonthlyLimit 150)) 5 False)
        `shouldBe` "agent 114%"
      rPercent (render now Nothing 83 (Just (MonthlyLimit 150)) 5 False) `shouldBe` "114%"
    it "carries the provisional marker" $
      rPercent (render now Nothing 83 (Just (MonthlyLimit 150)) 5 True) `shouldBe` "~114%"
    it "uses spent-plus-remaining as the denominator for a prepaid balance" $
      -- \$1 now + $0.50/day over 17.5 days = $9.75 against an allowance of $7.42
      rPercent (render now Nothing 1 (Just (Remaining 6.42)) 0.5 False) `shouldBe` "131%"
    it "has no percentage to show when unmetered" $ do
      rPercent (render now (Just "x") 5 Nothing 1 False) `shouldBe` "x —"
      rProjected (render now Nothing 5 Nothing 1 False) `shouldBe` 0

  describe "paint" $ do
    it "uses tmux markup, which is not ANSI" $
      paint "tmux" 1.5 "x" `shouldBe` "#[fg=red]x#[default]"
    it "uses ANSI when asked" $
      paint "ansi" 0.5 "x" `shouldBe` "\ESC[32mx\ESC[0m"
    it "leaves the string alone by default" $
      paint "none" 2 "x" `shouldBe` "x"

  describe "completeSpec" $ do
    it "lists pass: entries from a fake password store, re-attaching the prefix" $ do
      dir <- mkFakeStore
      setEnv "PASSWORD_STORE_DIR" dir
      sort <$> completeSpec "pass:"
        `shouldReturn` sort ["pass:api/requesty/main", "pass:api/requesty/sub/agent", "pass:api/openrouter/key"]
    it "treats a bare token as a pass entry, like parseSpec" $ do
      dir <- mkFakeStore
      setEnv "PASSWORD_STORE_DIR" dir
      _ <- completeSpec "api/requesty"
      True `shouldBe` True
    it "completes env: names after the prefix" $ do
      setEnv "BURNRATE_TEST_VAR" "x"
      cs <- completeSpec "env:BURNRATE_TEST_"
      cs `shouldContain` ["env:BURNRATE_TEST_VAR"]
    it "re-attaches file: and keeps directories" $ do
      dir <- mkFakeStore
      -- completeSpec drops up to and including the mode colon, then walks `dir`
      sort <$> completeSpec ("file:" <> dir <> "/api/requesty")
        `shouldReturn` [ "file:" <> dir <> "/api/requesty/main.gpg",
                         "file:" <> dir <> "/api/requesty/sub/"
                       ]

  describe "parseAcct" $ do
    it "reads a Requesty key, its group, and its sparse daily history" $
      case parseAcct requestyFixture of
        Nothing -> expectationFailure "failed to parse"
        Just a -> do
          acLabel a `shouldBe` "agent"
          acSpent a `near` 83.38
          acBudget a `shouldBe` Just (MonthlyLimit 150)
          acGroup a `shouldBe` Just "g1"
          M.toList (acHist a) `shouldBe` [(fromGregorian 2026 8 3, 5.5)]
    it "reads an OpenRouter key with a resetting limit as a monthly budget" $
      case parseAcct orLimitFixture of
        Nothing -> expectationFailure "failed to parse"
        Just a -> do
          acBudget a `shouldBe` Just (MonthlyLimit 100)
          acSpent a `near` 42.5
          acGroup a `shouldBe` Nothing
    it "falls back to the account balance when the key limit is null" $
      case parseAcct orBalanceFixture of
        Nothing -> expectationFailure "failed to parse"
        Just a -> case acBudget a of
          Just (Remaining b) -> b `near` 12.4
          other -> expectationFailure ("expected a balance, got " <> show other)
    it "rejects an unknown provider" $
      parseAcct (object ["provider" .= ("wat" :: String)]) `shouldBe` Nothing

-- Money arrives as JSON strings from Requesty and as numbers from OpenRouter.
requestyFixture, orLimitFixture, orBalanceFixture :: Value
requestyFixture =
  object
    [ "provider" .= ("requesty" :: String),
      "self"
        .= object
          [ "name" .= ("agent" :: String),
            "monthly_spend" .= ("83.38" :: String),
            "monthly_limit" .= ("150" :: String),
            "group" .= object ["id" .= ("g1" :: String)]
          ],
      "usage"
        .= object
          ["usage" .= object ["2026-08-03" .= object ["spend" .= ("5.5" :: String)]]]
    ]
orLimitFixture =
  object
    [ "provider" .= ("openrouter" :: String),
      "key"
        .= object
          [ "data"
              .= object
                [ "label" .= ("or" :: String),
                  "usage_monthly" .= (42.5 :: Double),
                  "limit" .= (100 :: Double),
                  "limit_reset" .= ("monthly" :: String),
                  "limit_remaining" .= (57.5 :: Double)
                ]
          ],
      "credits" .= object []
    ]
orBalanceFixture =
  object
    [ "provider" .= ("openrouter" :: String),
      "key"
        .= object
          [ "data"
              .= object
                [ "label" .= ("t" :: String),
                  "usage_monthly" .= (6.0 :: Double),
                  "limit" .= (Nothing :: Maybe Double)
                ]
          ],
      "credits"
        .= object
          [ "data"
              .= object
                ["total_credits" .= (50 :: Double), "total_usage" .= (37.6 :: Double)]
          ]
    ]
