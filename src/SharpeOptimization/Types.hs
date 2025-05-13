{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Centralized type definitions for financial simulation.
-- | Outer matrices use boxed vectors (`Vector`) for flexibility,
-- | while inner rows use unboxed vectors (`U.Vector Double`) for performance.

module SharpeOptimization.Types
  ( Price, 
    Return,
    Weights,
    PricesRow, 
    ReturnsRow, 
    PriceMatrix , ReturnMatrix , CovarianceMatrix,
    Best,
    Sharpe,
    Stocks
  ) where

import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U

-- | A price value (e.g., closing price of a stock)
type Price = Double

-- | A return value (e.g., daily return)
type Return = Double

-- | Sharpe Ratio value
type Sharpe = Double

-- | List of asset names, e.g., ["AAPL", "MSFT", "GOOG"]
type Stocks = [String]

-- | A portfolio weight vector (one weight per asset), unboxed for performance
type Weights = U.Vector Double

-- | A single row of price data for multiple assets on a given day.
-- | Each element represents the price of one asset on that day.
type PricesRow = U.Vector Price

-- | A single row of return data for multiple assets on a given day.
-- | Each element represents the return of one asset on that day.
type ReturnsRow = U.Vector Return

-- | Entire price dataset: a boxed vector of unboxed rows.
-- | Each inner row corresponds to one day's prices for all assets.
type PriceMatrix = V.Vector PricesRow

-- | Entire return dataset: a boxed vector of unboxed rows.
-- | Each inner row corresponds to one day's returns for all assets.
type ReturnMatrix = V.Vector ReturnsRow

-- | Covariance matrix: a boxed vector of rows (each row is unboxed).
-- | Each inner vector is a row of covariances between asset returns.
type CovarianceMatrix = V.Vector (U.Vector Double)

-- | Represents the best portfolio found:
--   - The Sharpe Ratio value
--   - The list of asset names selected
--   - The corresponding weight vector
type Best = (Sharpe, Stocks, Weights)
