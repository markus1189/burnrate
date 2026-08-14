-- | Provider adapters. Fetching is 'IO' and parsing is pure, so the cache can
--   store raw responses and a cache hit costs no network and no re-derivation
--   drift: the same bytes parse to the same account.
{-# LANGUAGE OverloadedStrings #-}

module Burnrate.Provider
  ( fetchRaw
  , parseAcct
  , apiGet
  , num
  , at
  , objectPairs
  ) where

import           Burnrate.Estimate (monthStart)
import           Burnrate.Types
import           Control.Applicative ((<|>))
import           Control.Lens ((^?))
import           Data.Aeson (Value (Object), decode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import           Data.Aeson.Lens (key, _Double, _String)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time
import           Network.HTTP.Simple
import           Text.Read (readMaybe)

-- | A GET that may carry a body, which Requesty's usage endpoint requires.
apiGet :: String -> Text -> Maybe Value -> IO Value
apiGet url tok body = do
  base <- parseRequest url
  resp <- httpLBS . setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 tok]
        . maybe id setRequestBodyJSON body $ setRequestMethod "GET" base
  let b = getResponseBody resp
  case getResponseStatusCode resp of
    c | c >= 200 && c < 300 -> maybe (fail (url <> ": unparseable JSON")) pure (decode b)
      | otherwise -> fail ("HTTP " <> show c <> " " <> take 140 (BL.unpack b))

-- | Both providers report money as JSON strings in some fields and numbers in
--   others, with no discernible rule.
num :: Value -> Maybe Double
num v = (v ^? _String >>= readMaybe . T.unpack) <|> (v ^? _Double)

at :: Value -> K.Key -> Maybe Double
at v k = v ^? key k >>= num

fetchRaw :: Provider -> Text -> Day -> IO Value
fetchRaw Requesty tok today = do
  self  <- apiGet rq tok Nothing
  usage <- apiGet (rq <> "/usage") tok . Just $ object
             [ "start" .= stamp (monthStart today), "end" .= stamp (succ today)
             , "resolution" .= ("day" :: Text) ]
  pure $ object ["provider" .= ("requesty" :: Text), "self" .= self, "usage" .= usage]
  where
    -- 'self' is an undocumented alias for the key's own uuid, and unlike the
    -- uuid form it needs no manage permission.
    rq = "https://api-v2.requesty.ai/v1/manage/apikey/self"
    stamp d = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (UTCTime d 0)

fetchRaw OpenRouter tok _ = do
  k <- apiGet "https://openrouter.ai/api/v1/key" tok Nothing
  -- A null per-key limit means the account balance is the real constraint.
  c <- case k ^? key "data" . key "limit" >>= num of
         Just _  -> pure (object [])
         Nothing -> apiGet "https://openrouter.ai/api/v1/credits" tok Nothing
  pure $ object ["provider" .= ("openrouter" :: Text), "key" .= k, "credits" .= c]

parseAcct :: Value -> Maybe Account
parseAcct v = case v ^? key "provider" . _String of
  Just "requesty" -> do
    self <- v ^? key "self"
    pure Account
      { acLabel  = fromMaybe "requesty" (self ^? key "name" . _String)
      , acSpent  = fromMaybe 0 (self `at` "monthly_spend")
      , acBudget = MonthlyLimit <$> self `at` "monthly_limit"
      , acHist   = M.fromList
          [ (d, s)
          | (dk, e) <- objectPairs (v ^? key "usage" . key "usage")
          , Just d  <- [parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dk)]
          , Just s  <- [e `at` "spend"] ]
      , acGroup  = self ^? key "group" . key "id" . _String }
  Just "openrouter" -> do
    d <- v ^? key "key" . key "data"
    let cr     = v ^? key "credits" . key "data"
        resets = maybe False (not . T.null) (d ^? key "limit_reset" . _String)
        bal    = (-) <$> (cr >>= (`at` "total_credits")) <*> (cr >>= (`at` "total_usage"))
    pure Account
      { acLabel  = fromMaybe "openrouter" (d ^? key "label" . _String)
      , acSpent  = fromMaybe 0 (d `at` "usage_monthly")
      , acBudget = case (d `at` "limit", resets) of
          (Just l, True) -> Just (MonthlyLimit l)
          (Just _, _)    -> Remaining <$> d `at` "limit_remaining"
          (Nothing, _)   -> Remaining <$> bal
      , acHist   = M.empty  -- OpenRouter exposes no usage history at all
      , acGroup  = Nothing }
  _ -> Nothing

-- | lens-aeson has no un-indexed object-pair traversal; aeson's KeyMap is simpler.
objectPairs :: Maybe Value -> [(Text, Value)]
objectPairs (Just (Object m)) = map (\(k, x) -> (K.toText k, x)) (KM.toList m)
objectPairs _                 = []
