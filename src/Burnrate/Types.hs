-- | Core vocabulary. Deliberately free of aeson, HTTP and CLI types so that
--   the estimator and renderer can be tested without any of them.
module Burnrate.Types
  ( Provider (..)
  , Src (..)
  , Spec (..)
  , Budget (..)
  , Account (..)
  ) where

import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Data.Time (Day)

data Provider = Requesty | OpenRouter
  deriving (Eq, Show)

-- | Where a key comes from. Anything that can produce a token on stdout.
data Src = Pass String | Env String | File FilePath | Cmd String
  deriving (Eq, Show)

data Spec = Spec
  { spProv :: Maybe Provider  -- ^ Nothing means sniff it from the key prefix.
  , spSrc  :: Src
  , spRaw  :: String          -- ^ Verbatim argument; the cache and sample keys.
  } deriving (Eq, Show)

-- | 'MonthlyLimit' resets with the calendar month. 'Remaining' is a prepaid
--   balance and is therefore not bounded by the month at all — the distinction
--   decides whether a projection means \"pace\" or \"runway\".
data Budget = MonthlyLimit Double | Remaining Double
  deriving (Eq, Show)

data Account = Account
  { acLabel  :: Text
  , acSpent  :: Double          -- ^ Current calendar month, all providers.
  , acBudget :: Maybe Budget
  , acHist   :: Map Day Double  -- ^ Daily spend; sparse, and empty if the
                                --   provider offers no history at all.
  , acGroup  :: Maybe Text      -- ^ Provider-side group id, for shared budgets.
  } deriving (Eq, Show)
