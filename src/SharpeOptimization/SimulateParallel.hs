{-# LANGUAGE BangPatterns #-}
module SharpeOptimization.SimulateParallel
  ( simulateBestSharpeParallel ) where

import SharpeOptimization.Types
import SharpeOptimization.Statistics
import SharpeOptimization.Weights 
import SharpeOptimization.SimulateSequential

import           System.Random (StdGen, split)
import           Control.Parallel.Strategies
import           GHC.Conc (numCapabilities)

-----------------------------------------------------------------------
simulateBestSharpeParallel
  :: ReturnMatrix -> CovarianceMatrix -> [String]
  -> Int -> Int -> StdGen -> (Maybe Best, StdGen)
simulateBestSharpeParallel retM σ names k n gen0 =
  let μ        = muVector retM
      combos   = combinations k [0 .. length names - 1]
      gens     = splitMany (length combos) gen0
      chunkSz  = max 1 (length combos `div` (numCapabilities * 4))

      perCombo (idxs, g) =
        let (ws, _) = randomWeightVectorsPure n (length idxs) g
        in comboBest μ σ names idxs ws

      localBests = withStrategy (parListChunk chunkSz rdeepseq)
                    $ map perCombo (zip combos gens)

      best = foldr better Nothing localBests
      genOut = snd (split gen0)
  in (best, genOut)

-----------------------------------------------------------------------
-- Helper to split RNGs safely for parallelism
-----------------------------------------------------------------------
splitMany :: Int -> StdGen -> [StdGen]
splitMany 0 _ = []
splitMany k g = let (g1,g2) = split g in g1 : splitMany (k-1) g2
