-- | Random weight generator for portfolio optimization.
--   Generates k weights that sum to 1, with each weight ≤ 20%.
--   The result resembles a Dirichlet sample and is suitable for constrained sampling.

module SharpeOptimization.Weights (randomWeights) where

import SharpeOptimization.Types
import Control.Monad.Random
import qualified Data.Vector.Unboxed as U

-- | Constraint: no single asset may have more than 20% allocation
maxPerAsset, tol :: Double
maxPerAsset = 0.20     -- maximum weight per asset
tol         = 1e-6     -- tolerance for floating point error in sum

-- | Generates a random weight vector of length k.
--   Ensures weights sum to 1 and are each ≤ maxPerAsset.
--   Retries until constraints are satisfied.
randomWeights :: (MonadRandom m) => Int -> m Weights
randomWeights k = loop
  where
    loop = do
      -- Sample k positive values in [1e-3, 1.0]
      xs <- replicateM k (getRandomR (1e-3, 1.0))
      let v = U.fromList xs
          s = U.sum v
          w = U.map (/ s) v  -- normalize to sum to 1
      if validWeights w
        then pure w
        else loop  -- retry if constraints not met

-- | Validates that weights sum to 1 (within tolerance) and are all ≤ maxPerAsset
validWeights :: Weights -> Bool
validWeights w =
  abs (U.sum w - 1) < tol &&  -- sum approximately 1
  U.all (<= maxPerAsset) w    -- no weight exceeds maximum
