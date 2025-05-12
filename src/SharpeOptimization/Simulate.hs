--   Module responsible for simulating portfolio optimizations.
--   For each combination of k assets, it generates n random portfolios,
--   computes their Sharpe Ratios, and keeps track of the best found.
--   Returns the maximum Sharpe Ratio, associated stock names, and weights.
module SharpeOptimization.Simulate (simulateBestSharpe) where

import SharpeOptimization.Types
import SharpeOptimization.Statistics
import SharpeOptimization.Weights

import Control.Monad.Random (MonadRandom)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad (foldM, when)

--   Simulates and selects the best Sharpe Ratio for all combinations of k assets.
--   For each combination, it tries n random portfolios.
--   Returns:
--     - Maximum Sharpe Ratio found
--     - Stock names used
--     - Weights for the optimal portfolio
simulateBestSharpe
  :: MonadRandom m
  => [StockName]                    --   Full list of stock names
  -> ReturnMatrix                   --   Full return matrix (days x assets)
  -> Int                            --   Number of assets to select (k)
  -> Int                            --   Number of random tries per combination (n)
  -> ExceptT String m (Double, [StockName], Weights)
simulateBestSharpe stocks returns k n = do
  let assetCount = length (head returns)

  when (k > assetCount) $
    throwE "k elements to be combined cannot exceed the number of assets"
  when (n < 1) $
    throwE "weight vector must be generated at least once"

  let allCombos = combinations k [0 .. assetCount - 1]

  result <- foldM (evaluateCombination returns n) Nothing allCombos

  case result of
    Nothing -> throwE "No valid Sharpe Ratio found."
    Just (score, chosenIdxs, chosenWeights) ->
      return (score, selectByIndexes chosenIdxs stocks, chosenWeights)


--   Evaluates all portfolios for one combination of asset indexes.
--   Keeps track of the best Sharpe Ratio across n trials.
evaluateCombination
  :: MonadRandom m
  => ReturnMatrix                                       --  Full return matrix (days x assets)
  -> Int                                                --  Number of random tries per combination (n)
  -> Maybe (Double, [Int], Weights)                     --  Current best Sharpe result
  -> [Int]                                              --  Asset indexes for the current combination
  -> ExceptT String m (Maybe (Double, [Int], Weights))  -- Updated best result
evaluateCombination returns n currentBest assetIndexes = do
  let selectedReturns = map (selectByIndexes assetIndexes) returns
      loop 0 best = return best
      loop k best = do
        updated <- evaluateTrial selectedReturns assetIndexes best
        loop (k - 1) updated

  loop n currentBest
  


--   Evaluates a single trial with randomly generated weights.
--   Updates the best result if a better Sharpe Ratio is found.
evaluateTrial
  :: MonadRandom m
  => ReturnMatrix                                       --  Selected returns for the current combination
  -> [Int]                                              --  Asset indexes for the current combination
  -> Maybe (Double, [Int], Weights)                     --  Current best Sharpe result
  -> ExceptT String m (Maybe (Double, [Int], Weights))  --  Updated best result
evaluateTrial selectedReturns assetIndexes currentBest = do
  weights <- randomWeights (length assetIndexes)

  case sharpeRatio selectedReturns weights of
    Nothing     -> return currentBest
    Just score  ->
      case currentBest of
        Nothing -> return $ Just (score, assetIndexes, weights)
        Just (bestScore, _, _) ->
          if score > bestScore
            then return $ Just (score, assetIndexes, weights)
            else return currentBest
