-- | Burn-rate estimation. Every function here is pure and total, which is the
--   point: three separate bugs in this logic shipped looking healthy before it
--   was extracted and tested.
module Burnrate.Estimate
  ( rate
  , estimate
  , histIntervals
  , sampleIntervals
  , provisional
  , allowance
  , monthStart
  , monthElapsed
  , monthLength
  ) where

import           Burnrate.Types
import qualified Data.Map.Strict as M
import           Data.Time

-- | Recency-weighted spend per day from @(ageDays, spend, elapsedDays)@
--   intervals. Weighting by elapsed time rather than by sample count means a
--   burst of idle polls cannot outvote one long busy interval.
rate :: Double -> [(Double, Double, Double)] -> Maybe Double
rate hl xs
  | tot <= 0  = Nothing
  | otherwise = Just (spend / tot)
  where w a   = 0.5 ** (a / hl)
        spend = sum [w a * s | (a, s, _) <- xs]
        tot   = sum [w a * t | (a, _, t) <- xs]

monthStart :: Day -> Day
monthStart d = let (y, m, _) = toGregorian d in fromGregorian y m 1

monthElapsed, monthLength :: UTCTime -> Double
monthElapsed t = fromIntegral (dom - 1) + realToFrac (utctDayTime t) / 86400
  where (_, _, dom) = toGregorian (utctDay t)
monthLength t = fromIntegral (gregorianMonthLength y m)
  where (y, m, _) = toGregorian (utctDay t)

-- | Completed days of the current month, zero-filled. Providers omit idle days
--   from their responses entirely, and averaging only the days they return
--   overstates the rate by however many days you did not work.
histIntervals :: Day -> M.Map Day Double -> [(Double, Double, Double)]
histIntervals today h =
  [ (fromIntegral (diffDays today d), M.findWithDefault 0 d h, 1)
  | d <- [monthStart today .. pred today] ]

-- | Deltas between consecutive samples of a cumulative counter. Non-monotonic
--   pairs are dropped: that is a month rollover, not negative spend.
sampleIntervals :: Double -> [(Double, Double)] -> [(Double, Double, Double)]
sampleIntervals now ss =
  [ ((now - t1) / 86400, v1 - v0, (t1 - t0) / 86400)
  | ((t0, v0), (t1, v1)) <- zip ss (drop 1 ss), t1 > t0, v1 >= v0 ]

-- | A prepaid balance converts to an equivalent monthly allowance — what has
--   been spent this month plus what is left — so it can join a grand total.
allowance :: Account -> Maybe Double
allowance a = case acBudget a of
  Just (MonthlyLimit l) -> Just l
  Just (Remaining b)    -> Just (acSpent a + b)
  Nothing               -> Nothing

-- | Is an aggregate's rate provisional? Only if the guessed part carries a
--   material share of it. An idle key with no history contributes nothing but a
--   caveat, and a caveat that always fires stops being read.
provisional :: [(Double, Bool)] -> Bool
provisional rs = guessed > 0.1 * sum (map fst rs) && guessed > 0
  where guessed = sum [r | (r, True) <- rs]

-- | Burn rate in $/day, and whether it had to fall back to a provisional guess.
--   Prefers real daily history, then the sampling log, then a flat month-to-date
--   run-rate.
estimate
  :: Double               -- ^ Half-life in days for the recency weighting.
  -> UTCTime              -- ^ Now.
  -> Double               -- ^ Now, as POSIX seconds, for the sample log.
  -> [(Double, Double)]   -- ^ Sampling log: @(epochSeconds, cumulativeSpend)@.
  -> Account
  -> (Double, Bool)
estimate hl now epoch samples a = case (histRate, sampleRate) of
    (Just x, _) -> (x, False)
    (_, Just x) -> (x, False)
    _           -> (if elapsed > 0 then acSpent a / elapsed else 0, True)
  where
    today   = utctDay now
    elapsed = monthElapsed now
    -- Today is partial, so estimate from completed days only. An empty history
    -- must fall through to samples rather than report a confident zero.
    histRate | M.null (acHist a) = Nothing
             | otherwise = rate hl (histIntervals today (acHist a))
    -- Two idle samples minutes apart imply a burn rate of zero and thus "never
    -- exhausts". Demand a real observation window before believing the log.
    sampleRate | sum [t | (_, _, t) <- ints] < 0.25 = Nothing
               | otherwise = rate hl ints
      where ints = sampleIntervals epoch samples
