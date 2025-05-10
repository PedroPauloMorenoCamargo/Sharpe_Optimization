module SharpeOptimization.Types (
    StockName,
    Price,
    Return,
    Weights,
    PricesRow,
    ReturnsRow,
    PriceMatrix,
    ReturnMatrix

) where

type StockName    = String
type Price        = Double
type Return       = Double
type Weights      = [Double]
type PricesRow    = [Price]
type ReturnsRow   = [Return]
type PriceMatrix  = [PricesRow]
type ReturnMatrix = [ReturnsRow]