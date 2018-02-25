{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}
module Main where
import           System.FilePath
import           System.Directory
import           System.Environment
import           Data.Semigroup
import           Data.GraphViz.Attributes
import           Data.GraphViz.Commands
import           Data.GraphViz.Types
import           Data.GraphViz.Types.Monadic
import qualified Data.GraphViz.Types.Generalised as G
import qualified Data.Text.Lazy                  as T

graphIdInt :: Int -> GraphID
graphIdInt = Num . Int

progGraph :: G.DotGraph T.Text
progGraph = digraph (Str "G") $ do

   cluster (graphIdInt 0) $ do
       graphAttrs [style filled, color LightGray]
       nodeAttrs [style filled, color White]
       "a0" --> "a1"
       "a1" --> "a2"
       "a2" --> "a3"
       graphAttrs [textLabel "process #1"]

   cluster (graphIdInt 1) $ do
       nodeAttrs [style filled]
       "b0" --> "b1"
       "b1" --> "b2"
       "b2" --> "b3"
       graphAttrs [textLabel "process #2", color Blue]

   "start" --> "a0"
   "start" --> "b0"
   "a1" --> "b3"
   "b2" --> "a3"
   "a3" --> "end"
   "b3" --> "end"

   node "start" [shape MDiamond]
   node "end" [shape MSquare]

defaultPath :: IO FilePath
defaultPath = do
  args <- getArgs
  if null args
  then getCurrentDirectory
  else return $ head args

main :: IO ()
main = do
  inPath <- defaultPath
  let outputPdfPath = inPath </> takeFileName inPath <> ".pdf"
  putStrLn ("output file path: " <> outputPdfPath)
  const () <$> runGraphviz progGraph Pdf outputPdfPath
