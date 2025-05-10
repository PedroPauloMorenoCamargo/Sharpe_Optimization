module Main where
import SharpeOptimization.Weights (randomWeights)
import Control.Monad.Random (evalRandIO)
import Control.Monad.Trans.Except (runExceptT)

main :: IO ()
main = do
  result <- evalRandIO $ runExceptT (randomWeights 5)
  case result of
    Left err -> putStrLn ("Erro: " ++ err)
    Right ws -> do
      putStrLn "Pesos válidos gerados:"
      print ws
