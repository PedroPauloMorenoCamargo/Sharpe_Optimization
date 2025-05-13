module SharpeOptimization.Weights
  ( randomWeightVectorsPure
  ) where

import SharpeOptimization.Types
import System.Random (StdGen, randomRs, split)
import qualified Data.Vector.Unboxed as U

-- | Máximo peso por ativo e tolerância de soma
maxPerAsset, tol :: Double
maxPerAsset = 1.0
tol         = 1e-6

-- | Gera um único vetor de pesos aleatórios com k ativos
randomWeightsPure :: Int -> StdGen -> (Weights, StdGen)
randomWeightsPure k gen = loop gen
  where
    loop g =
      let (g1, g2) = split g
          xs = take k $ randomRs (1e-3, 1.0) g1
          v  = U.fromList xs
          s  = U.sum v
          w  = U.map (/ s) v
      in if validWeights w
         then (w, g2)
         else loop g2

-- | Gera N vetores de pesos aleatórios com k ativos cada
randomWeightVectorsPure :: Int -> Int -> StdGen -> ([Weights], StdGen)
randomWeightVectorsPure n k gen = go n gen []
  where
    go 0 g acc = (reverse acc, g)
    go m g acc =
      let (w, g') = randomWeightsPure k g
      in go (m - 1) g' (w : acc)

-- | Verifica se o vetor de pesos é válido
validWeights :: Weights -> Bool
validWeights w =
  abs (U.sum w - 1) < tol &&
  U.all (<= maxPerAsset) w
