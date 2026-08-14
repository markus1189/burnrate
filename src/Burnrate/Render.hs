-- | Status-line rendering. One line per row, terse enough for a status bar.
module Burnrate.Render
  ( Report (..)
  , render
  , paint
  ) where

import           Burnrate.Estimate (monthElapsed, monthLength)
import           Burnrate.Types
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Text.Printf (printf)

data Report = Report
  { rLine      :: String
  , rSeverity  :: Double        -- ^ Pace; drives the colour thresholds.
  , rWhen      :: Maybe Double  -- ^ Days until the budget is gone, if ever.
  , rPercent   :: String        -- ^ Projected end-of-month spend as a percentage.
  , rProjected :: Double        -- ^ The same as a ratio, for colour thresholds.
  } deriving (Eq, Show)

-- | One status line. 'Nothing' for the label gives the bare aggregate form.
--   The presence of the arrow /is/ the warning: a row without one is not
--   projected to run out.
render
  :: UTCTime       -- ^ Now.
  -> Maybe Text    -- ^ Label, or Nothing for an aggregate row.
  -> Double        -- ^ Spend, month to date.
  -> Maybe Budget
  -> Double        -- ^ Burn rate, $/day.
  -> Bool          -- ^ Provisional?
  -> Report
render now mlbl spent budget r prov = Report line sev exhaust pctLine projRatio
  where
    dim     = monthLength now
    elapsed = monthElapsed now
    left    = dim - elapsed

    runway hd = if r > 0 && hd > 0 then Just (hd / r) else Nothing
    -- A monthly limit that survives to the reset never actually breaches; a
    -- prepaid balance has no reset to save it.
    exhaust = case budget of
      Just (MonthlyLimit l) -> runway (l - spent) >>= \d -> if d <= left then Just d else Nothing
      Just (Remaining b)    -> runway b
      Nothing               -> Nothing

    at' d = " → " <> formatTime defaultTimeLocale "%b %-d"
                       (addUTCTime (realToFrac (d * 86400)) now)
    tilde = if prov then "~" else ""
    tag s = maybe s (\l -> T.unpack l <> " " <> s) mlbl

    (line, sev) = case budget of
      Just (MonthlyLimit l) ->
        let pace = if l > 0 && elapsed > 0 then spent / (l * (elapsed / dim)) else 0
        in ( tag (unwords [money spent <> "/" <> money l, tilde <> printf "%.1fx" pace])
               <> maybe "" at' exhaust
           , pace )
      Just (Remaining b) ->
        -- No time-boxed budget means no pace; severity comes from the runway.
        ( tag (unwords [money b, "left"]) <> maybe "" at' exhaust
        , maybe 0 (\d -> 14 / max 0.1 d) exhaust )
      Nothing -> (tag (unwords [money spent, "mtd"]), 0)

    money :: Double -> String
    money v | v >= 10   = printf "$%.0f" v
            | otherwise = printf "$%.2f" v

    -- Where the month ends up, not where it stands: current spend plus the
    -- estimated rate over the days still to come. A prepaid balance has no
    -- monthly figure to divide by, so it uses the equivalent allowance —
    -- what has been spent plus what is left.
    denom = case budget of
      Just (MonthlyLimit l) -> Just l
      Just (Remaining b)    -> Just (spent + b)
      Nothing               -> Nothing
    projRatio = case denom of
      Just d | d > 0 -> (spent + r * left) / d
      _              -> 0
    pctLine = tag $ case denom of
      Just d | d > 0 -> tilde <> printf "%.0f%%" (100 * projRatio)
      _              -> "—"

-- | tmux interprets @#[...]@, not ANSI, in status output — hence two dialects.
paint :: String -> Double -> String -> String
paint st sev s = case st of
  "tmux" -> "#[fg=" <> c <> "]" <> s <> "#[default]"
  "ansi" -> "\ESC[" <> ansi <> "m" <> s <> "\ESC[0m"
  _      -> s
  where (c, ansi) | sev >= 1.3 = ("red", "31")
                  | sev >= 1.0 = ("yellow", "33")
                  | otherwise  = ("green", "32")
