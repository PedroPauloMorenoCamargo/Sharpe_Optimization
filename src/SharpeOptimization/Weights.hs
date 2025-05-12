--   Module responsible for generating random portfolio weight vectors.
--   Ensures the weights:
--     - sum to 1 (within numerical tolerance)
--     - are bounded by a maximum allocation per asset (e.g., 20%)
module SharpeOptimization.Weights (randomWeights) where

import Control.Monad.Random (MonadRandom, getRandomR, replicateM)
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import SharpeOptimization.Types (Weights)
import Control.Monad (liftM)

--   Maximum allowed allocation per asset (e.g., 20%)
maxPerAsset :: Double
maxPerAsset = 0.20

--   Numerical tolerance for sum-to-one check
tol :: Double
tol = 1e-6

--   Generates a random weight vector of length `n`.
--   The weights are sampled uniformly and normalized to sum to 1.
--   Constraints:
--     - n must be at least 6 (so it does not become an infinite loop)
--     - each weight is in the range [0,1]
--     - each weight <= maxPerAsset
--     - sum of weights ≈ 1 (within tolerance)
--   If constraints are not met, retry generation recursively.
randomWeights :: MonadRandom m => Int -> ExceptT String m Weights
randomWeights n
  | n < 6     = throwE "Weight vector must have at least 6 elements"
  | otherwise = generate
  where
    generate = do
      -- Generate n random numbers uniformly in [0,1]
      xs <- ExceptT $ fmap Right (replicateM n (getRandomR (0, 1)))

      -- Normalize the weights so they sum to 1
      let s  = sum xs
          ws = map (/ s) xs

      -- Validate and return, or retry if invalid
      if validWeights ws
        then return ws
        else generate

--   Validates that:
--     - weights sum approximately to 1
--     - no individual weight exceeds maxPerAsset
validWeights :: Weights -> Bool
validWeights ws =
  abs (sum ws - 1) < tol &&
  all (<= maxPerAsset) ws
