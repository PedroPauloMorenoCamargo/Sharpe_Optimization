{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module SharpeOptimization.Statistics
  ( combinations,
    selectByIndexesU,
    mean,
    muVector,
    covarianceMatrix,
    dotProductU,
    matVecU,
    sharpeRatioFast,
    priceMatrixToReturns,
    toPriceMatrix
  ) where

import SharpeOptimization.Types
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as G

----------------------------------------------------------------------
-- Combinatorics
----------------------------------------------------------------------

-- | Generates all possible combinations of 'k' elements from a given list.
--   If 'k' is greater than the length of the list, returns an empty list.
--   Example:
--     combinations 2 [1,2,3] == [[1,2], [1,3], [2,3]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k (x:xs)
  | k < 0     = []
  | otherwise = map (x:) (combinations (k-1) xs) ++ combinations k xs

----------------------------------------------------------------------
-- Vector utilities for unboxed data
----------------------------------------------------------------------

-- | Selects a subset of elements from an unboxed vector, given a list of indices.
--   Example:
--     selectByIndexesU [0,2] (U.fromList [10.0, 20.0, 30.0]) == U.fromList [10.0, 30.0]
selectByIndexesU :: U.Unbox a => [Int] -> U.Vector a -> U.Vector a
selectByIndexesU idxs v = U.backpermute v (U.fromList idxs)

-- | Computes the dot product of two unboxed vectors.
--   If vectors have different lengths, excess elements are ignored.
--   Example:
--     dotProductU (U.fromList [1.0, 2.0]) (U.fromList [3.0, 4.0]) == 11.0
dotProductU :: U.Vector Double -> U.Vector Double -> Double
dotProductU v1 v2 = U.sum (U.zipWith (*) v1 v2)

-- | Multiplies a matrix (represented as a boxed vector of unboxed rows)
--   by a column vector. Each row is dotted with the input vector.
--   Returns the resulting unboxed vector.
--   Used for computing portfolio variance and other linear algebra ops.
matVecU :: V.Vector (U.Vector Double) -> U.Vector Double -> U.Vector Double
matVecU m v = U.generate (V.length m) $ \i -> dotProductU (m V.! i) v

----------------------------------------------------------------------
-- Mean vector and covariance matrix
----------------------------------------------------------------------

-- | Computes the mean of a generic vector.
--   Returns 0.0 for an empty vector.
mean :: (G.Vector v Double) => v Double -> Double
mean vec | G.null vec = 0
         | otherwise  = G.sum vec / fromIntegral (G.length vec)

-- | Computes the mean return (μ vector) for each asset (column-wise).
--   Input: return matrix of shape (days × assets)
--   Output: unboxed vector of asset-wise average returns.
muVector :: ReturnMatrix -> U.Vector Double
muVector rm =
  let !cols = transpose rm
  in U.generate (V.length cols) $ \j -> mean (cols V.! j)

-- | Computes the sample covariance matrix (Σ) from a matrix of returns.
--   Centralizes each column by subtracting the mean, then calculates:
--     Σᵢⱼ = (Xᵢ ⋅ Xⱼ) / (n - 1)
--   where Xᵢ is the centralized column vector for asset i.
covarianceMatrix :: ReturnMatrix -> CovarianceMatrix
covarianceMatrix returns =
  let !r     = centralizeMatrix returns
      !rt    = transpose r
      nRows  = fromIntegral (V.length r) - 1
      nCols  = V.length rt
  in V.generate nCols $ \i ->
       let xi = rt V.! i
       in U.generate nCols $ \j ->
            dotProductU xi (rt V.! j) / nRows


----------------------------------------------------------------------
-- Sharpe Ratio calculation
----------------------------------------------------------------------

-- | Computes the annualized Sharpe Ratio using precomputed mean (μ),
--   covariance matrix (Σ), and a weight vector (w).
--   Returns Nothing if portfolio volatility is zero.
--   Formula:
--     Sharpe = (μᵗ·w · 252) / (√(wᵗ·Σ·w) · √252)
sharpeRatioFast :: U.Vector Double -> CovarianceMatrix -> Weights -> Maybe Sharpe
sharpeRatioFast mu sigma w
  | U.length mu /= U.length w = Nothing
  | V.length sigma /= U.length w = Nothing
  | otherwise =
      let !expectedReturn = dotProductU mu w       -- μᵗ · w
          !σw             = matVecU sigma w        -- Σ · w
          !variance       = dotProductU w σw       -- wᵗ · Σ · w
          !epsilon        = 1e-6
      in if variance < epsilon
         then Nothing
         else 
           let annualizedSharpe = (expectedReturn * 252) / (sqrt variance * sqrt 252)
           in Just annualizedSharpe



----------------------------------------------------------------------
-- Internal matrix helpers
----------------------------------------------------------------------

-- | Centralizes a column vector (zero-mean) by subtracting its mean.
centralizeColumn :: ReturnsRow -> ReturnsRow
centralizeColumn col = let m = mean col in U.map (\x -> x - m) col

-- | Centralizes each column of a matrix by subtracting the mean,
--   and returns the matrix in the original orientation (days × assets).
centralizeMatrix :: ReturnMatrix -> ReturnMatrix
centralizeMatrix mat = transpose $ V.map centralizeColumn (transpose mat)


-- | Transposes a boxed vector of unboxed vectors.
--   Converts rows to columns and vice versa.
--   Input: Vector n × m
--   Output: Vector m × n
transpose :: U.Unbox a => V.Vector (U.Vector a) -> V.Vector (U.Vector a)
transpose rows
  | V.null rows = V.empty
  | otherwise   =
      let nCols = U.length (V.head rows)
      in V.generate nCols $ \j ->
           U.generate (V.length rows) $ \i -> (rows V.! i) U.! j

----------------------------------------------------------------------
-- Data transformation
----------------------------------------------------------------------

-- | Converts a list of lists (e.g. from CSV) into a price matrix.
--   Each inner list becomes an unboxed row of the matrix.
--   Used to transform raw parsed data into a structured format.
toPriceMatrix :: [[Double]] -> PriceMatrix
toPriceMatrix rows = V.fromList $ map U.fromList rows

-- | Converts a matrix of prices (days × assets) to a matrix of returns.
--   Each row of the output represents the returns between two consecutive days.
--   Output will have one fewer row than the input.
priceMatrixToReturns :: PriceMatrix -> ReturnMatrix
priceMatrixToReturns pm
  | V.length pm < 2 = V.empty
  | otherwise =
      V.zipWith
        (\prev next -> U.zipWith (\p0 p1 -> (p1 / p0) - 1) prev next)
        pm
        (V.tail pm)
