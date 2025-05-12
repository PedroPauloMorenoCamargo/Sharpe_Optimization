{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad              (forM_, when)
import           System.Exit                (exitFailure)
import           System.IO                  (hFlush, stdout)
import           Data.Time.Clock            (getCurrentTime, diffUTCTime)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector         as V

import           SharpeOptimization.DataLoader  
import           SharpeOptimization.Statistics
import           SharpeOptimization.Simulate
import           GHC.Conc                (getNumCapabilities)
import           Control.DeepSeq         (force)
import           Control.Exception       (evaluate)

------------------------------------------------------------
-- Prompt user for input, using a default value if input is empty
------------------------------------------------------------
promptDefault :: String -> String -> IO String
promptDefault msg def = do
  putStr $ msg ++ " [default: " ++ def ++ "]: "
  hFlush stdout
  input <- getLine
  return (if null input then def else input)


------------------------------------------------------------
-- Helper: Formats a Double with fixed number of decimal places
------------------------------------------------------------
showFFloat :: Int -> Double -> String -> String
showFFloat d x suffix = show (fromIntegral (round (x * f) :: Int) / f) ++ suffix
  where f = 10 ^ d :: Double


------------------------------------------------------------
-- Main program entry point
-- Prompts user for input, runs simulation, evaluates on secondary dataset
------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "=== Sharpe Optimization ==="

  caps <- getNumCapabilities
  putStrLn $ "🧵 GHC runtime using " ++ show caps ++ " thread(s)"
  when (caps == 1) $
    putStrLn "⚠️  Warning: Only 1 thread enabled. Use +RTS -N4 to parallelize."

  -- Prompt for input with defaults
  csvPath     <- promptDefault "Training CSV file path" "data/training.csv"
  resultPath  <- promptDefault "Result CSV file path" "data/result.csv"
  kStr        <- promptDefault "Number of assets to choose (k)" "25"
  nStr        <- promptDefault "Number of trials per combination (n)" "1000"

  case (reads kStr, reads nStr) of
    ([(k, "")], [(n, "")]) -> runApp csvPath resultPath k n
    _ -> putStrLn "❌ Invalid input. Please enter valid integers for k and n." >> exitFailure

------------------------------------------------------------
-- Executes the Sharpe ratio optimization using a training set,
-- then evaluates the resulting portfolio on a result dataset
------------------------------------------------------------
runApp :: FilePath -> FilePath -> Int -> Int -> IO ()
runApp csvPath resultPath k n = do
  eitherData <- runExceptT (loadStockData csvPath)
  case eitherData of
    Left err -> putStrLn ("❌ " ++ err) >> exitFailure
    Right (names, priceRows) -> do
      let m = length names
      if k < 1 || k > m
        then putStrLn $ "❌ Invalid k: must be between 1 and " ++ show m
        else do
          let priceMatrix  = toPriceMatrix priceRows
              returnMatrix = priceMatrixToReturns priceMatrix
              sigmaMatrix  = covarianceMatrix returnMatrix

          putStrLn $ "\n⏳ Running simulation over " ++ show k ++ "-asset portfolios (" ++ show n ++ " trials each)..."

          start <- getCurrentTime
          result <- simulateBestSharpeParallel returnMatrix sigmaMatrix names k n
          best <- evaluate (force result)
          end <- getCurrentTime

          let duration = realToFrac (diffUTCTime end start) :: Double

          case best of
            Nothing -> putStrLn "⚠️  No portfolio had non-zero volatility."
            Just (bestSR, chosenNames, weights) -> do
              putStrLn "\n✔️  Best Sharpe portfolio found:"
              putStrLn $ "    Sharpe Ratio : " ++ show bestSR
              putStrLn   "    Assets / Weights:"
              let ws = U.toList weights
              forM_ (zip chosenNames ws) $ \(nm, w) ->
                putStrLn $ "      " ++ nm ++ "  ->  " ++ show w

              putStrLn $ "\n⏱️  Optimization Elapsed time: " ++ showFFloat 2 duration " seconds"

              -- Evaluate same portfolio on a different dataset (result CSV)
              putStrLn "\n🔁 Evaluating same portfolio on result dataset..."
              resultData <- runExceptT (loadStockData resultPath)
              case resultData of
                Left err2 -> putStrLn ("❌ Failed to load result CSV: " ++ err2)
                Right (names', priceRows') -> do
                  let nameToIndex = \name -> maybe (-1) id (lookup name (zip names' [0..]))
                      indices = map nameToIndex chosenNames
                      valid   = all (>= 0) indices

                  if not valid
                    then putStrLn "⚠️  Some selected stocks not found in result CSV."
                    else do
                      let priceMatrix'  = toPriceMatrix priceRows'
                          returnMatrix' = priceMatrixToReturns priceMatrix'
                          mu'           = muVector returnMatrix'
                          sigma'        = covarianceMatrix returnMatrix'
                          muSubset      = selectByIndexesU indices mu'
                          sigmaSubset   = V.map (selectByIndexesU indices) $ V.backpermute sigma' (V.fromList indices)

                      case sharpeRatioFast muSubset sigmaSubset weights of
                        Nothing -> putStrLn "⚠️  New Sharpe Ratio: invalid (likely zero volatility)."
                        Just newSR -> do
                          putStrLn $ "✅ New Sharpe Ratio on result dataset: " ++ show newSR
                          if newSR > bestSR
                            then putStrLn "🥳 Improved Sharpe in result dataset!"
                            else putStrLn "😕 Worse or equal Sharpe in result dataset."
