{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Exception.Safe
import           System.FilePath
import           System.Directory
import           System.Environment
import           Data.Maybe
import           Data.Bifunctor
import qualified Database.Redis                  as R
import qualified Data.Map.Strict                 as M
import           Data.Foldable
import           Data.Semigroup
import           Data.GraphViz.Attributes
import           Data.GraphViz.Commands
import           Data.GraphViz.Types
import           Data.GraphViz.Types.Monadic
import qualified Data.GraphViz.Types.Generalised as G
import qualified Data.Text.Lazy                  as T
import qualified Data.Text.Lazy.Encoding         as TE
import qualified Data.ByteString                 as B
import           Data.ByteString.Lazy               (fromStrict)

prefixKeyBroader :: B.ByteString
prefixKeyBroader = "b:"

prefixKeyBroaderTxt :: T.Text
prefixKeyBroaderTxt = TE.decodeUtf8 $ fromStrict prefixKeyBroader

prefixKeyCat :: B.ByteString
prefixKeyCat = "category:"

prefixKeyCatTxt :: T.Text
prefixKeyCatTxt = TE.decodeUtf8 $ fromStrict prefixKeyCat

graphIdInt :: Int -> GraphID
graphIdInt = Num . Int

partialEither :: (MonadThrow m, Show l) => Either l r -> m r
partialEither (Left vl) = throwString $ show vl
partialEither (Right vr) = pure vr

type OneToMany a f = M.Map a (f a)

readRelOfOneToMany :: R.Connection -> B.ByteString -> IO (OneToMany T.Text [])
readRelOfOneToMany conn keyPattern = do
  ks <- partialEither =<< R.runRedis conn (R.keys keyPattern)
  vs <- mapM partialEither =<< R.runRedis conn (mapM R.smembers ks)
  let ksLazyText = map (TE.decodeUtf8 . fromStrict) ks
  let vsLazyText = map (map (TE.decodeUtf8 . fromStrict)) vs
  pure . M.fromList $ zip ksLazyText vsLazyText

clusterOfCategory :: (Foldable f) => T.Text -> f T.Text -> Dot T.Text
clusterOfCategory catRawName nodes = cluster (Str catName) $ do
  graphAttrs [style filled, color LightGray, textLabel catName]
  nodeAttrs [style filled, shape BoxShape, color White]
  mapM_ node' nodes
  where
    catName = stripPrefixIfHave prefixKeyCatTxt catRawName

stripPrefixIfHave :: T.Text -> T.Text -> T.Text
stripPrefixIfHave prefix txt = fromMaybe txt $ T.stripPrefix prefix txt

edgeToBroader :: (Foldable f) => OneToMany T.Text f -> Dot T.Text
edgeToBroader = mapM_ (spreadEdges . first stripBroaderPrefix) . M.toList
  where
    stripBroaderPrefix = stripPrefixIfHave prefixKeyBroaderTxt
    spreadEdges (u, d) = (u -->) `traverse_` d

drawGraph :: (Foldable f) => OneToMany T.Text f -> OneToMany T.Text f -> G.DotGraph T.Text
drawGraph articlesInCategory broaderRel = digraph (Str "G") $ do
  sequence_ . M.elems $ M.mapWithKey clusterOfCategory articlesInCategory
  edgeToBroader broaderRel

defaultPath :: IO FilePath
defaultPath = do
  args <- getArgs
  if null args
  then getCurrentDirectory
  else return $ head args

pdfPath :: FilePath -> FilePath
pdfPath dirPath = dirPath </> takeFileName dirPath <> ".pdf"

main :: IO ()
main = do
  inPath <- defaultPath
  let outputPdfPath = pdfPath inPath
  conn <- R.checkedConnect R.defaultConnectInfo
  putStrLn ("output file path: " <> outputPdfPath)
  articlesInCategory <- readRelOfOneToMany conn categoryPattern
  broaderRel <- readRelOfOneToMany conn broaderPattern
  const () <$> runGraphviz (drawGraph articlesInCategory broaderRel) Pdf outputPdfPath
  where
    categoryPattern = prefixKeyCat <> "*"
    broaderPattern = prefixKeyBroader <> "*"
