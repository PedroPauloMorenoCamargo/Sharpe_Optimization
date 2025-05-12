module Main where

import SharpeOptimization.DataLoader
import SharpeOptimization.Simulate 
import SharpeOptimization.Statistics 

import Control.Monad.Trans.Except (runExceptT)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  args <- getArgs
  let path = case args of
        (p:_) -> p
        []    -> "data/training.csv"

  putStrLn $ "📂 Loading data from: " ++ path

  result <- runExceptT $ do
    -- Step 1: Load stock names and price matrix from CSV
    (stockNames, priceMatrix) <- loadStockData path

    -- Step 2: Convert prices to returns
    let returnMatrix = pricesToReturns priceMatrix

    -- Step 3: Run simulation using 5 assets and 100 random portfolios
    liftIO $ putStrLn "🚀 Starting portfolio simulation..."
    simulateBestSharpe stockNames returnMatrix 28    10000

  case result of
    Left err -> putStrLn $ "❌ Error: " ++ err
    Right (sharpe, names, weights) -> do
      putStrLn "\n✅ Best Portfolio Found:"
      putStrLn $ "📈 Sharpe Ratio: " ++ show sharpe
      putStrLn $ "🏦 Stocks:       " ++ show names
      putStrLn $ "⚖️  Weights:      " ++ show weights
