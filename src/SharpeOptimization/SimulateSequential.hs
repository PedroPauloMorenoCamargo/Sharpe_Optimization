-- =====================================================================
--  SharpeOptimization.Simulate  (pure, threads StdGen)
-- =====================================================================
{-# LANGUAGE BangPatterns #-}
module SharpeOptimization.SimulateSequential
  ( better
  , stepSharpe
  , comboBest
  , sliceSigma
  , simulateBestSharpeSequential
  ) where


-----------------------------------------------------------------------
--  Imports
-----------------------------------------------------------------------

import           SharpeOptimization.Types
import           SharpeOptimization.Statistics
                   ( combinations
                   , muVector
                   , selectByIndexesU
                   , sharpeRatioFast )
import           SharpeOptimization.Weights      (randomWeightVectorsPure)

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import           System.Random                       (StdGen)


-----------------------------------------------------------------------
--  Pure helpers
-----------------------------------------------------------------------

-- | Keep the better of two candidates (Nothing == −∞).
better :: Maybe Best -> Maybe Best -> Maybe Best
better Nothing  x         = x
better x        Nothing   = x
better x@(Just (s1,_,_)) y@(Just (s2,_,_))
  | s2 > s1                   = y
  | otherwise                 = x

-- | Evaluate a single weight vector and update the best Sharpe.
stepSharpe :: U.Vector Double -> CovarianceMatrix -> [String] -> Weights -> Maybe Best -> Maybe Best
stepSharpe μ σ names w acc =
  case sharpeRatioFast μ σ w of
    Nothing  -> acc
    Just !sh -> better acc (Just (sh, names, w))

-- | Evaluate all weights for a single combo.
comboBest :: U.Vector Double     -- ^ μ all
          -> CovarianceMatrix    -- ^ Σ all
          -> [String]            -- ^ all names
          -> [Int]               -- ^ indices in this combo
          -> [Weights]           -- ^ candidate weights list
          -> Maybe Best
comboBest μAll σAll names idxs ws =
  let μSub    = selectByIndexesU idxs μAll
      σSub    = sliceSigma idxs σAll
      nameSub = map (names !!) idxs
  in  foldr (stepSharpe μSub σSub nameSub) Nothing ws

-- | Extract Σ sub-matrix for selected indices.
sliceSigma :: [Int] -> CovarianceMatrix -> CovarianceMatrix
sliceSigma idxs σ =
  let pickRow i = selectByIndexesU idxs (σ V.! i)
  in  V.fromList (map pickRow idxs)

-- | Process a single combination, returning the best Sharpe and new RNG.
evalCombination :: U.Vector Double -> CovarianceMatrix -> [String] -> Int -> Int -> StdGen -> [Int] -> (Maybe Best, StdGen)
evalCombination μ σ names n k g idxs =
  let (ws, g') = randomWeightVectorsPure n k g
      result   = comboBest μ σ names idxs ws
  in (result, g')

-- | Process all combinations sequentially.
evalAllCombinations :: U.Vector Double -> CovarianceMatrix -> [String] -> Int -> Int -> StdGen -> [[Int]] -> (Maybe Best, StdGen)
evalAllCombinations _ _ _ _ _ g [] = (Nothing, g)
evalAllCombinations μ σ names n k g (c:cs) =
  let (r1, g1) = evalCombination μ σ names n k g c
      (r2, g2) = evalAllCombinations μ σ names n k g1 cs
  in (better r1 r2, g2)

-----------------------------------------------------------------------
--  Main pure driver
-----------------------------------------------------------------------

-- | Sequential exhaustive search.  Returns the best Sharpe and the
--   updated RNG.
--   • k – portfolio size
--   • n – random portfolios per combination
simulateBestSharpeSequential
  :: ReturnMatrix      -- ^ R : daily returns
  -> CovarianceMatrix  -- ^ Σ : covariance matrix
  -> [String]          -- ^ asset names
  -> Int               -- ^ k
  -> Int               -- ^ n
  -> StdGen            -- ^ random generator in
  -> (Maybe Best, StdGen)  -- ^ result + updated gen
simulateBestSharpeSequential retM σ names k n gen0 =
  let μ      = muVector retM
      combos = combinations k [0 .. length names - 1]
  in evalAllCombinations μ σ names n k gen0 combos