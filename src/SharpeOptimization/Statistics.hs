--   Module providing basic statistical and linear algebra utilities.
--   These functions are used for portfolio optimization calculations.
module SharpeOptimization.Statistics ( 
    combinations,
    selectByIndexes,
    sharpeRatio
  ) where

import SharpeOptimization.Types

--   Returns all possible combinations of k elements from a list.
--   The elements retain their original order in each combination.
--   If k > length xs, returns an empty list.
--   Example: combinations 2 [1,2,3] == [[1,2],[1,3],[2,3]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _      = [[]]
combinations _ []     = []
combinations k xs | k > length xs = []
combinations 1 xs = map (:[]) xs
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

--   Computes the dot product of two vectors (element-wise product summed).
--   Returns 0 if either list is empty.
--   Example: dotProduct [1,2] [3,4] == 1*3 + 2*4 == 11
dotProduct :: [Double] -> [Double] -> Double
dotProduct [] _ = 0
dotProduct _ [] = 0
dotProduct (x:xs) (y:ys) = x * y + dotProduct xs ys

--   Multiplies a matrix (represented as a list of rows) by a column vector. Matrix * Vector.
--   For each row in the matrix, computes the dot product with the vector.
--   Assumes the vector has the same length as the number of columns in the matrix.
--   Returns a vector with one element per row.
--   Example: matrixVectorProduct [[1,2],[3,4]] [5,6] == [1*5+2*6, 3*5+4*6] == [17,39]
matrixVectorProduct :: [[Double]] -> [Double] -> [Double]
matrixVectorProduct [] _ = []
matrixVectorProduct _ [] = []
matrixVectorProduct (x:xs) v = dotProduct x v : matrixVectorProduct xs v

--   Multiplies a row vector by a matrix (list of columns). Vector * Matrix.
--   The matrix is transposed before multiplication to align columns as rows.
--   Assumes the vector has the same length as the number of rows in the matrix.
--   Returns a vector where each element is the dot product between the vector and a column of the matrix.
--   Example: vectorMatrixProduct [1,2] [[1,2],[3,4]] == [1*1 + 2*3, 1*2 + 2*4] == [7,10]
vectorMatrixProduct :: [Double] -> [[Double]] -> [Double]
vectorMatrixProduct [] _ = []
vectorMatrixProduct _ [] = []
vectorMatrixProduct v m = map (dotProduct v) (transpose m)

--   Computes the arithmetic mean (average) of a list of Doubles. u = Σx / n
--   Assumes the list is non-empty.
--   Example: mean [1.0, 2.0, 3.0] == 2.0
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

--   Selects elements from a list using a list of indexes.
--   Assumes that all indexes are within bounds of the list.
--   Example: selectByIndexes [0,2] ["a","b","c"] == ["a","c"]
selectByIndexes :: [Int] -> [a] -> [a]
selectByIndexes idxs xs = map (xs !!) idxs

--   Transposes a matrix (list of lists), converting rows into columns.
--   Assumes the matrix is rectangular.
--   Example: transpose [[1,2],[3,4]] == [[1,3],[2,4]]
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

--   Centralizes a list of Doubles by subtracting the mean from each element.
--   The resulting list will have a mean of approximately zero.
--   Example: centralizeColumn [1.0, 2.0, 3.0] == [-1.0, 0.0, 1.0]
centralizeColumn :: [Double] -> [Double]
centralizeColumn xs = 
  let m = mean xs
  in map (\x -> x - m) xs

--   Centralizes each column of a matrix independently.
--   Transposes the matrix, applies centralization to each column, then transposes back.
--   Example: centralizeMatrix [[1,2],[3,4]]
centralizeMatrix :: ReturnMatrix -> ReturnMatrix
centralizeMatrix mat = transpose $ map centralizeColumn (transpose mat)

--   Computes the sample covariance matrix of a return matrix.
--   The return matrix must be in the form of rows = days, columns = assets.
--   First centralizes the matrix, then computes Σ = (1 / (n - 1)) * Rᵗ * R.
--   Returns a square matrix with dimensions = number of assets.
--   Example: covarianceMatrix [[0.01, 0.02], [0.015, 0.025]] ≈ [[2.5e-05, 2.5e-05], [2.5e-05, 2.5e-05]]
covarianceMatrix :: ReturnMatrix -> [[Double]]
covarianceMatrix mat =
  let r = centralizeMatrix mat
      rt = transpose r
      n = fromIntegral (length r)
      denom = n - 1
  in [ [ dotProduct xi yj / denom | yj <- rt ] | xi <- rt ]

--   Computes the annualized volatility of a portfolio.
--   Volatility is defined as σ = sqrt(wᵀ * Σ * w) * sqrt(252), where Σ is the covariance matrix and w is the weight vector.
--   Example: annualizedVolatility [[0.01, 0.02], [0.015, 0.025]] [0.5, 0.5] ≈ 0.01118
annualizedVolatility :: ReturnMatrix -> Weights -> Double
annualizedVolatility returns weights =
  let sigma = covarianceMatrix returns
      vol = sqrt $ dotProduct weights $ matrixVectorProduct sigma weights 
  in vol * sqrt 252

--   Computes the annualized return of a portfolio.
--   Uses weighted average of asset returns, then multiplies by 252 to annualize. mean(R * w) * 252.
--   Example: annualizedReturn [[0.01, 0.02], [0.015, 0.025]] [0.5, 0.5] ≈ 0.50375
annualizedReturn :: ReturnMatrix -> Weights -> Double
annualizedReturn returns weights =
  let rp = matrixVectorProduct returns weights
      avg_rp = mean rp
  in avg_rp * 252

--   Computes the Sharpe Ratio of a portfolio.
--   Defined as: Sharpe = (annualized return) / (annualized volatility)
--   Returns Nothing if the volatility is zero (to avoid division by zero).
--   Assumes a zero risk-free rate and daily returns.
--   Example: sharpeRatio [[0.01, 0.02], [0.03, 0.01]] [0.5, 0.5] ≈ Just 63.5
sharpeRatio :: ReturnMatrix -> Weights -> Maybe Double
sharpeRatio returns weights =
  let vol = annualizedVolatility returns weights
      ret = annualizedReturn returns weights
  in if vol == 0
        then Nothing
        else Just (ret / vol)
