{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import           Database.Redis
import qualified Data.Vector as V
import           Data.Text    (Text, split)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           GHC.Generics (Generic)
import           GHC.Conc (numCapabilities)
import           Data.Csv
import           Data.Char (ord)
import           Data.Either
import           Data.Monoid
import           Data.Ratio
import           System.Exit
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC
import           Control.Concurrent.Async
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Exception.Safe
import           System.IO (hFileSize, withFile, IOMode(ReadMode))

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

articleKeyHeader = (<>) "article:"
separateTab = defaultDecodeOptions {decDelimiter = fromIntegral (ord '\t')}

type YDataMonad = IO

collectArticlesToRedis :: Connection -> WikiEnTSV -> YDataMonad ()
collectArticlesToRedis conn tsvLine = do
  let categories = B.split (fromIntegral . ord $ ';') . category $ tsvLine
      addRelationOfCategoryToArticle = (flip sadd) ([entry tsvLine]) . articleKeyHeader
  liftIO . runRedis conn $ mapM_ addRelationOfCategoryToArticle categories

gigaByte = 1024 ^ 3
handleRange h offset segSize bufferSize =
  CB.sourceHandleRangeWithBuffer h (Just offset) (Just segSize) bufferSize

main :: IO ()
main = do
  withFile "wikipedia-en.txt" ReadMode $ \h -> do
    fileSize <- hFileSize h
    let readSizeOfSegments = fileSize `div` (toInteger numCapabilities)
    mapConcurrently_ seqYData
      . map (\ratioOffset -> handleRange h (floor $ ratioOffset * (realToFrac fileSize)) readSizeOfSegments gigaByte)
      . take numCapabilities $ enumFromThenTo @Double 0 invCap 1
  where
    invCap :: Double
    invCap = (realToFrac 1) / (realToFrac numCapabilities)
    seqYData :: ConduitM () B.ByteString YDataMonad () -> IO ()
    seqYData yDataSeqqer = do
      conn <- checkedConnect defaultConnectInfo
      runConduit
        $  yDataSeqqer
        .| CB.lines
        .| CC.map decodeToWikiEnTSV
        .| rightOrDie
        .| CC.filter ((== 0) . namespaceID) -- 0 is article. see https://en.wikipedia.org/wiki/Wikipedia:Namespace
        .| CC.mapM_ (collectArticlesToRedis conn)
    decodeToWikiEnTSV :: B.ByteString -> Either String (V.Vector WikiEnTSV)
    decodeToWikiEnTSV = decodeWith separateTab NoHeader . BL.fromStrict
    rightOrDie :: (MonadThrow m, MonadIO m) => ConduitM (Either String (V.Vector WikiEnTSV)) WikiEnTSV m ()
    rightOrDie =
      await >>= \case
      Nothing -> pure ()
      Just x  -> case x of
                   Right l | V.length l == 1 -> yield . V.head $ l
                   Left err -> pure ()
                   Right l -> do
                     liftIO . putStrLn $
                       "Detect more than one parse by cassava at a time. check following contents: " <> show l
                     CC.yieldMany l
