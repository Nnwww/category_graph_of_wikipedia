{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
module Main where
import           Database.Redis
import qualified Data.Vector as V
import           Data.Text    (Text, split)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           GHC.Generics (Generic)
import           Data.Csv
import           Data.Char (ord)
import           Data.Either
import           Data.Monoid
import           System.Exit
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Control.Exception.Safe

data WikiEnTSV = WikiEnTSV
  { entryID            :: Int
  , namespaceID        :: Int
  , entry              :: B.ByteString
  , hiragana           :: B.ByteString
  , author             :: B.ByteString
  , createdDate        :: B.ByteString
  , lastestUpdatedDate :: B.ByteString
  , numOfEdit          :: Int
  , category           :: B.ByteString
  , redirect1          :: B.ByteString
  } deriving (Generic, Show)

instance FromRecord WikiEnTSV
instance ToRecord WikiEnTSV

articleKeyHeader = (<> "article:")
separateTab = defaultDecodeOptions {decDelimiter = fromIntegral (ord '\t')}

collectArticlesToRedis :: (MonadResource m) => Connection -> WikiEnTSV -> m ()
collectArticlesToRedis conn tsvLine = liftIO $ do
  let categories = B.split (fromIntegral . ord $ ';') . category $ tsvLine
      addRelationOfCategoryToArticle = (flip sadd) ([entry tsvLine]) . articleKeyHeader
  liftIO . runRedis conn $ do
    mapM_ addRelationOfCategoryToArticle categories

main :: IO ()
main = do
  conn <- checkedConnect defaultConnectInfo
  runConduitRes
    $ CB.sourceFile "wikipedia-en.txt"
    .| CB.lines
    .| CC.map decodeToWikiEnTSV
    .| CC.filter isRight
    .| CC.mapM rightOrDie
    .| CC.mapM_ (collectArticlesToRedis conn)
  where
    decodeToWikiEnTSV :: B.ByteString -> Either String (V.Vector WikiEnTSV)
    decodeToWikiEnTSV = decodeWith separateTab NoHeader . BL.fromStrict
    rightOrDie :: (MonadThrow m, MonadResource m) => Either String (V.Vector WikiEnTSV) -> m WikiEnTSV
    rightOrDie = \case
      Right l | V.length l == 1 -> pure . V.head $ l
      Left err -> throwString $ "decode tsv error:" <> err
      otherwise -> throwString $ "conduit's flow rate error."
