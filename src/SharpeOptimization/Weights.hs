module SharpeOptimization.Weights( randomWeightVectorsPure) where

import SharpeOptimization.Types
import System.Random (StdGen, randomRs, split)
import qualified Data.Vector.Unboxed as U

-- | Maximum allowed weight per asset (0.2 = 20%)
-- | Tolerance for checking if the total sum of weights is approximately 1
maxPerAsset, tol :: Double
maxPerAsset = 0.2
tol         = 1e-6

-- | Generates a single random weight vector for k assets.
-- | The weights are normalized to sum to 1 and are bounded by maxPerAsset.
-- | Rejection sampling is used: keeps generating until a valid vector is found.
--
-- Arguments:
--   k   - Number of assets
--   gen - Initial random generator
--
-- Returns:
--   (Weights, StdGen) - A valid weight vector and the updated generator
randomWeightsPure :: Int -> StdGen -> (Weights, StdGen)
randomWeightsPure k gen = loop gen
  where
    loop g =
      let (g1, g2) = split g
          xs = take k $ randomRs (1e-3, 1.0) g1  -- Avoid zero weights
          v  = U.fromList xs
          s  = U.sum v
          w  = U.map (/ s) v  -- Normalize weights to sum to 1
      in if validWeights w
         then (w, g2)
         else loop g2  -- Try again with a new generator

-- | Generates N random weight vectors, each with k assets.
-- | All generated vectors satisfy the constraints in 'validWeights'.
--
-- Arguments:
--   n   - Number of weight vectors to generate
--   k   - Number of assets per vector
--   gen - Initial random generator
--
-- Returns:
--   ([Weights], StdGen) - List of valid weight vectors and final generator
randomWeightVectorsPure :: Int -> Int -> StdGen -> ([Weights], StdGen)
randomWeightVectorsPure n k gen = go n gen []
  where
    go 0 g acc = (reverse acc, g)
    go m g acc =
      let (w, g') = randomWeightsPure k g
      in go (m - 1) g' (w : acc)

-- | Checks if a weight vector is valid.
--   - Total sum must be approximately 1 (within tolerance)
--   - No individual weight exceeds the maximum allowed
--
-- Argument:
--   w - Weight vector to validate
--
-- Returns:
--   True if valid, False otherwise
validWeights :: Weights -> Bool
validWeights w =
  abs (U.sum w - 1) < tol &&
  U.all (<= maxPerAsset) w
