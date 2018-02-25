{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}
module MainViz where
import Data.GraphViz.Attributes
import Data.GraphViz.Commands
import Data.GraphViz.Types
import Data.GraphViz.Types.Monadic
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

main :: IO ()
main = const () <$> runGraphviz progGraph Pdf "."
