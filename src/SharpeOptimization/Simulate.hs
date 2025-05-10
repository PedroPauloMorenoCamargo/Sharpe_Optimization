module SharpeOptimization.Simulate (simulation) where

import Control.Monad (foldM)
import Control.Monad.Random (evalRandIO)
import SharpeOptimization.Weights (randomWeights)
import SharpeOptimization.Stats (portfolioReturn, portfolioVol, sharpeRatio)
import SharpeOptimization.Utils (selectByIndices, filterMatrixByIndices)
import SharpeOptimization.Statistics (combinations)

-- Executa a simulação sobre todas as combinações de k ações, gerando pesos aleatórios válidos e escolhendo o melhor Sharpe Ratio.
simulation :: Int -> [String] -> [[Double]] -> IO ([String], [Double], Double)
simulation k names returns = do
  let allIndices = [0 .. length names - 1]
      combos     = combinations k allIndices

  best <- foldM (tryCombination names returns k) Nothing combos

  case best of
    Just result -> return result
    Nothing     -> fail "Nenhuma combinação válida encontrada"

-- Avalia uma única combinação de índices de ações e atualiza o melhor Sharpe Ratio
tryCombination :: [String] -> [[Double]] -> Int -> Maybe ([String], [Double], Double) -> [Int] -> IO (Maybe ([String], [Double], Double))
tryCombination names returns k bestSoFar idxs = do
  let selectedNames   = selectByIndices idxs names
      selectedReturns = filterMatrixByIndices idxs returns

  weights <- evalRandIO (randomWeights k)
  let ret = portfolioReturn selectedReturns weights
      vol = portfolioVol selectedReturns weights
      sr  = sharpeRatio 0.05 ret vol

  let candidate = (selectedNames, weights, sr)

  return $ Just $ maybe candidate (maxBySharpe candidate) bestSoFar

-- | Compara dois resultados e mantém o de maior Sharpe Ratio
maxBySharpe :: ([String], [Double], Double) -> ([String], [Double], Double) -> ([String], [Double], Double)
maxBySharpe a@(_, _, sr1) b@(_, _, sr2)
  | sr1 > sr2 = a
  | otherwise = b
