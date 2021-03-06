{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}
module MainSepa where
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Bifunctor
import qualified Data.ByteString              as B
import           Data.Conduit
import qualified Data.Conduit.Binary          as CB
import qualified Data.Conduit.Combinators     as CC
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import           Database.Redis
import           System.IO                    (IOMode (ReadMode), withFile)
import           System.Environment           (getArgs)

data WikiEnTSV = WikiEnTSV
  { entryID            :: Int
  , namespaceID        :: Int
  , entry              :: T.Text
  , ailias             :: T.Text
  , author             :: T.Text
  , createdDate        :: T.Text
  , lastestUpdatedDate :: T.Text
  , numOfEdit          :: Int
  , category           :: T.Text
  , redirect1          :: T.Text
  } deriving (Show)

articleKeyHeader :: B.ByteString -> B.ByteString
articleKeyHeader = (<>) "category:"

type YDataMonad = StateT YDataState IO
type YDataState = Int

collectArticlesToRedis :: Connection -> WikiEnTSV -> YDataMonad ()
collectArticlesToRedis conn tsvLine = do
  let categories = T.splitOn ";" . category $ tsvLine
      addRelationOfCategoryToArticle = flip sadd [TE.encodeUtf8 $ entry tsvLine]
        . articleKeyHeader . TE.encodeUtf8
  liftIO . runRedis conn $ mapM_ addRelationOfCategoryToArticle categories

lineProcess :: String -> IO ()
lineProcess fileName = withFile fileName ReadMode $ (seqYData . CB.sourceHandle)

seqYData :: ConduitM () B.ByteString YDataMonad () -> IO ()
seqYData yDataSeqqer = do
  conn <- checkedConnect defaultConnectInfo
  numOfLine <- flip execStateT 0 . runConduit
    $  yDataSeqqer
    .| CB.lines
    .| decodeToWikiEnTSV
    .| reportErrorAndIgnoreIt
    .| CC.filter ((== 0) . namespaceID) -- Namespace 0 is article. see https://en.wikipedia.org/wiki/Wikipedia:Namespace
    .| CC.mapM_ (collectArticlesToRedis conn)

  putStrLn . ("The number of line processed: " <> ) . show $ numOfLine

last' :: T.Text -> Maybe Char
last' t | T.length t > 0 = Just $ T.last t
last' _ = Nothing

equalLastChar :: Char -> T.Text -> Maybe Bool
equalLastChar lastChar = fmap (== lastChar) . last'

parseWikiEnTSV :: T.Text -> Either SomeException WikiEnTSV
parseWikiEnTSV lineMaybeWithNewLine =
  splitTSV =<< unsnocIfEqLastChar '\r' =<< unsnocIfEqLastChar '\n' lineMaybeWithNewLine
  where
    unsnocIfEqLastChar :: Char -> T.Text -> Either SomeException T.Text
    unsnocIfEqLastChar lastChar line =
      case equalLastChar lastChar line of
        Just True  -> pure . fst . fromJust . T.unsnoc $ line
        Just False -> pure line
        Nothing    -> throwString "Parse Error: input is an empty."
    splitTSV line =
      let texts = T.splitOn "\t" line
          numTexts = length texts in
        if numTexts /= 10 then
          throwString $
          "Parse Error: too " <>
          (if numTexts > 10 then "many"  else "few") <>
          " field in a record. (" <> show numTexts <> " fields)\n" <>
          "input record: " <> show texts
        else
          Right WikiEnTSV { entryID            = read . T.unpack $ texts !! 0
                          , namespaceID        = read . T.unpack $ texts !! 1
                          , entry              = texts !! 2
                          , ailias             = texts !! 3
                          , author             = texts !! 4
                          , createdDate        = texts !! 5
                          , lastestUpdatedDate = texts !! 6
                          , numOfEdit          = read . T.unpack $ texts !! 7
                          , category           = texts !! 8
                          , redirect1          = texts !! 9
                          }

awaitJust :: Monad m => (i -> ConduitM i o m ()) -> ConduitM i o m ()
awaitJust f =
  await >>= \case
    Nothing -> pure ()
    Just x -> f x

decodeToWikiEnTSV :: (MonadState YDataState m)
  => ConduitM B.ByteString (Either SomeException WikiEnTSV) m ()
decodeToWikiEnTSV = do
  lift $ modify' (+1)
  awaitJust $ \line -> do
    if B.null line then pure ()
                   else yield $ parseWikiEnTSV
                        =<< (first toException . TE.decodeUtf8')
                        =<< pure line
    decodeToWikiEnTSV

reportErrorAndIgnoreIt :: (Exception e, MonadState YDataState m, MonadIO m)
                       => ConduitM (Either e WikiEnTSV) WikiEnTSV m ()
reportErrorAndIgnoreIt =
  awaitJust $ \v -> logic v >> reportErrorAndIgnoreIt
  where
    logic = \case
      Right l -> yield l
      Left err -> do
        lineNum <- lift $ Control.Monad.State.get
        liftIO . putStrLn $ "parse error at line " <> show lineNum <> ": " <> show err

main :: IO ()
main = do
  args <- getArgs
  lineProcess $ if length args == 0 then "wikipedia-en.txt" else head args
