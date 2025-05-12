module Main where

import SharpeOptimization.Statistics
import Text.Printf (printf)

main :: IO ()
main = do
  let testCases =
        [ ("Caso 1: carteira equilibrada", returns1, weights1)
        , ("Caso 2: ativos idênticos", returns2, weights2)
        , ("Caso 3: carteira negativa", returns3, weights3)
        , ("Caso 4: ativo volátil concentrado", returns4, weights4)
        ]

  mapM_ runTest testCases

  where
    runTest (label, returns, weights) = do
      putStrLn $ "\n🔹 " ++ label
      case sharpeRatio returns weights of
        Just sr -> printf "Sharpe Ratio: %.6f\n" sr
        Nothing -> putStrLn "Sharpe Ratio: indefinido (volatilidade zero)"

    returns1 = [[0.01, 0.02], [0.02, 0.03], [0.015, 0.025]]
    weights1 = [0.5, 0.5]

    returns2 = [[0.02, 0.02], [0.02, 0.02], [0.02, 0.02]]
    weights2 = [0.5, 0.5]

    returns3 = [[-0.01, -0.02], [-0.03, -0.01], [-0.02, -0.03]]
    weights3 = [0.5, 0.5]

    returns4 = [[0.10, 0.01], [-0.10, 0.01], [0.10, 0.01]]
    weights4 = [1.0, 0.0]
