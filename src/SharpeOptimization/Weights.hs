module SharpeOptimization.Weights (randomWeights) where

import Control.Monad.Random (MonadRandom, getRandomR, replicateM)
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import SharpeOptimization.Types (Weights)
import Control.Monad (liftM)


maxPerAsset, tol :: Double
maxPerAsset = 0.20   
tol         = 1e-6   


randomWeights :: MonadRandom m => Int -> ExceptT String m Weights
randomWeights n
  | n < 6 =  throwE "Weight vector must have at least 6 elements"
  | otherwise = generate
  where
    generate = do
      xs <- ExceptT $ fmap Right (replicateM n (getRandomR (0, 1)))
      let s  = sum xs
          ws = map (/ s) xs
      if validWeights ws
        then return ws
        else generate


validWeights :: Weights -> Bool
validWeights ws = abs (sum ws - 1) < tol && all (<= maxPerAsset) ws
