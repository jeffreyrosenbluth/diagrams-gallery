{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Data.Tree
import           Data.Foldable
import           Data.List (zipWith4)
import           Diagrams.Prelude
import           Diagrams.Backend.Cairo.CmdLine

data SNode = SNode Double Double Turn Turn Int (Colour Double)



subSection :: Double -> Double -> Turn -> Turn -> Int -> (Colour Double)
           -> Diagram Cairo R2
subSection r m a1 a2 n c
  = mconcat $ iterateN n (rotate theta) w
    where
      theta = (toTurn a2 - toTurn a1) / (fromIntegral n)
      w = annularWedge (m + r) r a1 (a1 + theta) # lc white # fc c # lw 0.008

toTree m x (Node _ ts) q1 q2 c
  = Node (SNode x m q1 q2 n c) ts'
      where
        n = length ts
        d =  (toTurn q2 - toTurn q1) / (fromIntegral n)
        qs = [toTurn q1 + ((fromIntegral i) * d ) | i <- [0..n]]
        fs = toTree m (m + x)
        ts' = zipWith4 fs ts (take (n-1) qs) (drop 1 qs) colors
        colors = [ lightcoral, lightseagreen, paleturquoise, lightsteelblue
                 , plum, violet, coral, honeydew]

sunburst (Node (SNode r m a1 a2 n c) ts)
  = subSection r m a1 a2 n c <> (foldMap sunburst ts)

t = unfoldTree (\n -> (0, replicate n (n-1))) 6
s = toTree 0.3 1 t 0 1 lightgray

example = sunburst s <> circle 1 # fc white # lw 0

main = defaultMain $ example # centerXY # pad 1.1