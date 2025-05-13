{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | Centralised type aliases.  Outer matrices stay boxed
--   (Vector of rows) but every row is now an *unboxed* `U.Vector Double`.
module SharpeOptimization.Types
  ( Price, Return
  , Weights
  , PricesRow , ReturnsRow
  , PriceMatrix , ReturnMatrix , CovarianceMatrix
  , Best
  ) where

import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as U

type Price   = Double
type Return  = Double
type Weights = U.Vector Double          -- unboxed

type PricesRow  = U.Vector Price        -- days → price per asset
type ReturnsRow = U.Vector Return       -- days → return per asset

type PriceMatrix     = V.Vector PricesRow      -- boxed outer / unboxed inner
type ReturnMatrix    = V.Vector ReturnsRow
type CovarianceMatrix = V.Vector (U.Vector Double)


-- | Best Sharpe (Sharpe, asset names, weight vector)
type Best = (Double, [String], Weights)