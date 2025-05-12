{-# LANGUAGE BangPatterns #-}

-- | Module responsible for running a parallel simulation to find
-- | the best Sharpe Ratio among all combinations of k stocks,
-- | using n trials with random weights per combination.
-- | The public function `simulateBestSharpeParallel` handles randomness and IO.
-- | The core logic is fully pure and parallelized via `Control.Parallel.Strategies`.

module SharpeOptimization.Simulate
  ( simulateBestSharpeParallel ) where

-- Internal modules with financial types and utilities
import SharpeOptimization.Types
import SharpeOptimization.Statistics
  ( combinations, muVector, selectByIndexesU
  , sharpeRatioFast )
import SharpeOptimization.Weights  (randomWeights)

-- Vectors for efficient numeric operations
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U

-- Parallel strategies for pure computation
import Control.Parallel.Strategies

-- For randomness and IO monads
import Control.Monad.Random   (MonadRandom, replicateM)
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Conc               (numCapabilities)

-------------------------------------------------------------------------------
-- | Pure simulation function: performs the entire computation in a pure,
-- | referentially transparent way. Given:
-- | - μ: expected returns vector
-- | - Σ: covariance matrix
-- | - names: list of stock names
-- | - weights: list of n precomputed weight vectors
-- | - combos: list of index combinations (choose k out of N stocks)
-- |
-- | Returns the best Sharpe ratio found, including the associated stock names
-- | and the corresponding weights.
-------------------------------------------------------------------------------
simulateBestSharpePure :: U.Vector Double                 -- ^ μ: expected return vector
  -> CovarianceMatrix               -- ^ Σ: full covariance matrix
  -> [String]                       -- ^ Stock names (indexed)
  -> [Weights]                      -- ^ List of n weight vectors (one for each trial)
  -> [[Int]]                        -- ^ All combinations of k indices
  -> Maybe (Double, [String], Weights) -- ^ Best Sharpe result found
simulateBestSharpePure muAll sigma names weights combos =
    foldr pickBest Nothing bestList
  where
    -- Compute size of chunks for parallel evaluation.
    -- We aim for roughly 4× more chunks than CPU cores for better granularity.
    chunkSize  = max 1 (length combos `div` (numCapabilities * 4))

    -- Evaluate all combinations in parallel using the chosen strategy
    bestList   = withStrategy (parListChunk chunkSize rdeepseq)
                              (map evalCombo combos)

    -- Evaluates a single combination of stock indices by trying all weights
    evalCombo idxs =
      let μ          = selectByIndexesU idxs muAll        -- Subvector μ for selected assets
          σ          = sliceSigma idxs sigma              -- Submatrix Σ for selected assets
          nameSubset = map (names !!) idxs                -- Names of the selected assets
      in  foldr (trySharpe μ σ nameSubset) Nothing weights

    -- Tries one weight vector and updates the best Sharpe ratio if applicable
    trySharpe μ σ ns w acc =
      case sharpeRatioFast μ σ w of
        Nothing -> acc  -- Skip if Sharpe ratio is undefined (e.g., zero volatility)
        Just sr -> case acc of
          Nothing -> Just (sr, ns, w)         -- First valid result
          Just (sr', _, _) | sr > sr' -> Just (sr, ns, w)  -- Update if better
          _ -> acc                            -- Keep the current best

    -- Picks the best result between two candidates
    pickBest Nothing  b = b
    pickBest a@(Just (sr, _, _)) (Just (sr', _, _))
      | sr > sr'       = a
    pickBest a _       = a


-------------------------------------------------------------------------------
-- | Public function that handles randomness and parallel computation.
-- | It orchestrates the simulation by:
-- |  1. Generating all combinations of k stock indices from N available.
-- |  2. Computing the mean returns vector (μ) from the return matrix.
-- |  3. Generating n random weight vectors with constraints.
-- |  4. Calling the pure simulation function to find the best result.
-- |
-- | This function is impure due to randomness (weights), but all parallel
-- | computation is delegated to the pure core (`simulateBestSharpePure`).
-------------------------------------------------------------------------------
simulateBestSharpeParallel :: (MonadIO m, MonadRandom m)
  => ReturnMatrix              -- ^ R: daily returns matrix (days × assets)
  -> CovarianceMatrix          -- ^ Σ: covariance matrix (assets × assets)
  -> [String]                  -- ^ List of stock names
  -> Int                       -- ^ k: number of assets to select in each portfolio
  -> Int                       -- ^ n: number of random portfolios to try per combination
  -> m (Maybe (Double, [String], Weights)) -- ^ Best Sharpe result (if any)
simulateBestSharpeParallel rm sigma names k n
  -- Validate bounds: must choose at least 1 and at most all assets
  | k < 1 || k > length names || n < 1 = pure Nothing
  | otherwise = do
      -- Step 1: generate all index combinations of k from N
      let combos = combinations k [0 .. length names - 1]
          -- Step 2: compute the expected returns vector μ
          μ      = muVector rm

      -- Step 3: generate n random weight vectors satisfying portfolio constraints
      weights <- replicateM n (randomWeights k)

      -- Step 4: run the pure, parallel simulation
      pure $ simulateBestSharpePure μ sigma names weights combos


-- | Extracts the k×k submatrix from the full Σ matrix using the selected indices.
sliceSigma :: [Int] -> CovarianceMatrix -> CovarianceMatrix
sliceSigma idxs σ =
  let iV = V.fromList idxs
      iU = U.fromList idxs
  in V.map (`U.backpermute` iU) (V.backpermute σ iV)
