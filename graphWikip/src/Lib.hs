module Lib
    ( OneToMany
    , partialEither
    , dig
    , entryInCategory
    , consCatsGraphUntilLevel
    , consCatsGraphUntilName
    ) where
import           Control.Exception.Safe
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as HS
import           Data.Semigroup
import           Data.Tree
import qualified Database.Redis as R

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
    query = direction isBroader <> category

entryInCategory :: R.Connection -> B.ByteString -> IO [B.ByteString]
entryInCategory conn category =
  partialEither =<< R.runRedis conn (R.smembers query)
  where
    query = "category:" <> category

data InfoConsCatTree a = InfoConsCatTree
  { cctConn       :: R.Connection
  , cctIsBroader  :: Bool
  , rootCat       :: B.ByteString
  , reachedCats   :: HS.HashSet B.ByteString
  , currentCat    :: B.ByteString
  , loopParam     :: a
  }

digup :: (InfoConsCatTree a -> InfoConsCatTree a)
      -> (InfoConsCatTree a -> Bool)
      -> InfoConsCatTree a
      -> IO [InfoConsCatTree a]
digup loopParamUpdater endCond old@InfoConsCatTree{..} =
  if endCond old then pure []
  else do
    !digged <- filter (`HS.member` reachedCats) <$> dig cctConn cctIsBroader currentCat
    let reachedCats' = foldr HS.insert reachedCats digged
    let new = loopParamUpdater $! old { reachedCats = reachedCats' }
    pure $ map (\nextCat -> new { currentCat = nextCat }) digged

consCatsGraphUntilLevel :: R.Connection
                        -> Bool
                        -> Word
                        -> B.ByteString
                        -> IO (Tree B.ByteString)
consCatsGraphUntilLevel conn isBroader maxLevel rootCategory =
  unfoldTreeM_BF consSubTree
  InfoConsCatTree { cctConn      = conn
                  , cctIsBroader = isBroader
                  , rootCat      = rootCategory
                  , reachedCats  = HS.insert rootCategory HS.empty
                  , currentCat   = rootCategory
                  , loopParam    = 0
                  }
  where
    consSubTree :: InfoConsCatTree Word -> IO (B.ByteString, [InfoConsCatTree Word])
    consSubTree info@InfoConsCatTree{currentCat} = (currentCat,) <$> digup nextLevel exceedLevel info
    nextLevel old@InfoConsCatTree{..} = old { loopParam = loopParam + 1 }
    exceedLevel InfoConsCatTree{loopParam} = loopParam >= maxLevel

consCatsGraphUntilName :: R.Connection
                       -> Bool
                       -> B.ByteString
                       -> B.ByteString
                       -> IO (Tree B.ByteString)
consCatsGraphUntilName conn isBroader rootCategory targetCategory =
  unfoldTreeM_BF consSubTree
  InfoConsCatTree { cctConn      = conn
                  , cctIsBroader = isBroader
                  , rootCat      = rootCategory
                  , reachedCats  = HS.insert rootCategory HS.empty
                  , currentCat   = rootCategory
                  , loopParam    = targetCategory
                  }
  where
    consSubTree :: InfoConsCatTree B.ByteString -> IO (B.ByteString, [InfoConsCatTree B.ByteString])
    consSubTree info@InfoConsCatTree{currentCat} = (currentCat,) <$> digup id reachTarget info
    reachTarget InfoConsCatTree{reachedCats} =  HS.member targetCategory reachedCats
