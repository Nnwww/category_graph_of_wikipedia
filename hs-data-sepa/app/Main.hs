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
import           System.Environment (getArgs)

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

type YDataMonad = StateT YDataState IO
type YDataState = Int

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
  args <- getArgs
  lineProcess $ if length args == 1 then read @Int $ head args else numCapabilities
  where
    lineProcess :: Int -> IO ()
    lineProcess numProcess =
      let invCap = 1 / (realToFrac numProcess) in
        withFile "wikipedia-en.txt" ReadMode $ \h -> do
          fileSize <- hFileSize h
          let readSizeOfSegments = fileSize `div` (toInteger numProcess)
          mapConcurrently_ seqYData
            . map (\ratioOffset -> handleRange h (floor $ ratioOffset * (realToFrac fileSize)) readSizeOfSegments gigaByte)
            . take numProcess $ enumFromThenTo @Double 0 invCap 1
    seqYData :: ConduitM () B.ByteString YDataMonad () -> IO ()
    seqYData yDataSeqqer = do
      conn <- checkedConnect defaultConnectInfo
      (flip evalStateT 0) . runConduit
        $  yDataSeqqer
        .| CB.lines
        .| CC.mapM decodeToWikiEnTSV
        .| rightOrDie
        .| CC.filter ((== 0) . namespaceID) -- 0 is article. see https://en.wikipedia.org/wiki/Wikipedia:Namespace
        .| CC.mapM_ (collectArticlesToRedis conn)
    decodeToWikiEnTSV :: B.ByteString -> YDataMonad (Either String (V.Vector WikiEnTSV))
    decodeToWikiEnTSV line = do
      modify' (+1)
      pure . decodeWith separateTab NoHeader $ BL.fromStrict line
    rightOrDie :: (MonadThrow m, MonadState YDataState m, MonadIO m)
      => ConduitM (Either String (V.Vector WikiEnTSV)) WikiEnTSV m ()
    rightOrDie =
      await >>= \case
      Nothing -> pure ()
      Just x  -> case x of
                   Right l | V.length l == 1 -> yield . V.head $ l
                   Left err -> do
                     lineNum <- Control.Monad.State.get
                     throwString $ "parse error at line " <> show lineNum <> ": " <> err
                   Right l -> do
                     liftIO . putStrLn $
                       "Detect more than one parse by cassava at a time. check following contents: " <> show l
                     CC.yieldMany l
