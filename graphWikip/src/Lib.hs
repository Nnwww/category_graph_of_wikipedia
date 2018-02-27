module Lib
    ( OneToMany
    , partialEither
    , dig
    , entryInCategory
    , search_category
    ) where

import           Control.Exception.Safe
import qualified Database.Redis                  as R

import qualified Data.HashMap.Strict             as M
import           Data.Tree
import           Data.Semigroup
import qualified Data.ByteString                 as B

type OneToMany a f = M.HashMap a (f a)

partialEither :: (MonadThrow m, Show l) => Either l r -> m r
partialEither (Left vl) = throwString $ show vl
partialEither (Right vr) = pure vr

direction :: Bool -> B.ByteString
direction isBroader = if isBroader then "b:" else "n:"

dig :: R.Connection -> Bool -> B.ByteString -> IO [B.ByteString]
dig conn isBroader category =
  partialEither =<< R.runRedis conn (R.smembers query)
  where
    query = (direction isBroader) <> category

entryInCategory :: R.Connection -> B.ByteString -> IO [B.ByteString]
entryInCategory conn category =
  partialEither =<< R.runRedis conn (R.smembers query)
  where
    query = "category:" <> category


search_category :: R.Connection
                -> Bool
                -> Word
                -> B.ByteString
                -> IO (Tree B.ByteString)
search_category conn isBroader max_distance category =
  unfoldTreeM_BF consSubTree (0, category)
  where
    consSubTree (dist, cat) | dist >= max_distance = pure (cat, [])
    consSubTree (dist, cat) = do
      digged <- dig conn isBroader cat
      pure (cat, map (dist + 1,) $ digged)
