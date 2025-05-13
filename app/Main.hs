{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- Base / utility imports
import           Control.Exception          (evaluate)
import           Control.DeepSeq            (force)
import           Control.Monad              (forM_)
import           Control.Monad.Trans.Except (runExceptT)
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           System.Exit                (exitFailure)
import           System.IO                  (hFlush, stdout)
import           System.Random              (getStdGen)

-- Vectors
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as U

-- Project modules
import           SharpeOptimization.DataLoader
import           SharpeOptimization.Statistics
import           SharpeOptimization.SimulateSequential
                     (simulateBestSharpeSequential)
import           SharpeOptimization.SimulateParallel
                     (simulateBestSharpeParallel)
import GHC.Conc (getNumCapabilities)

------------------------------------------------------------
-- Prompt helper with defaults
------------------------------------------------------------
promptDefault :: String -> String -> IO String
promptDefault msg def = do
  putStr $ msg ++ " [default: " ++ def ++ "]: "
  hFlush stdout
  input <- getLine
  pure (if null input then def else input)

------------------------------------------------------------
-- Simple fixed-precision printer
------------------------------------------------------------
showFFloat :: Int -> Double -> String -> String
showFFloat d x suffix = show (fromIntegral (round (x * f) :: Int) / f) ++ suffix
  where f = 10 ^ d :: Double

------------------------------------------------------------
-- Entry point
------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "=== Sharpe Optimization ==="

  -- Gather parameters -------------------------------------------------
  csvPath     <- promptDefault "Training CSV file path" "data/training.csv"
  resultPath  <- promptDefault "Result CSV file path"   "data/result.csv"
  kStr        <- promptDefault "Number of assets to choose (k)" "25"
  nStr        <- promptDefault "Trials per combination (n)"     "1000"
  parStr      <- promptDefault "Parallel? (1 = yes, 0 = no)"     "1"

  case (reads kStr, reads nStr, reads parStr) of
    ([(k,"")], [(n,"")], [(parFlag,"")]) ->
        runApp csvPath resultPath k n (parFlag /= (0 :: Int))
    _ -> putStrLn "❌ Invalid numeric input." >> exitFailure

------------------------------------------------------------
-- Core runner
------------------------------------------------------------
runApp :: FilePath -> FilePath -> Int -> Int -> Bool -> IO ()
runApp csvPath resultPath k n useParallel = do
  -- Load training data ----------------------------------------------
  eitherData <- runExceptT (loadStockData csvPath)
  case eitherData of
    Left err -> putStrLn ("❌ " ++ err) >> exitFailure
    Right (names, priceRows) -> do
      let m = length names
      if k < 1 || k > m
        then putStrLn $ "❌ Invalid k: must be between 1 and " ++ show m
        else do
          -- Pre-compute training statistics --------------------------
          let priceMatrix   = toPriceMatrix priceRows
              returnMatrix  = priceMatrixToReturns priceMatrix
              sigmaMatrix   = covarianceMatrix returnMatrix
              simFn | useParallel = simulateBestSharpeParallel
                    | otherwise   = simulateBestSharpeSequential
              modeLabel = if useParallel then "parallel" else "sequential"

          coreCount <- getNumCapabilities
          putStrLn $ "\n🧠 Using " ++ show coreCount ++ " CPU core(s)"
          putStrLn $ "⏳ Running " ++ modeLabel ++
                    " simulation over " ++ show k ++
                    "-asset portfolios (" ++ show n ++ " trials each)..."


          -- Run optimisation ----------------------------------------
          gen0   <- getStdGen
          start  <- getCurrentTime
          let (result, _) = simFn returnMatrix sigmaMatrix names k n gen0
          best   <- evaluate (force result)
          end    <- getCurrentTime
          let elapsed = realToFrac (diffUTCTime end start) :: Double

          -- Present optimisation outcome ----------------------------
          case best of
            Nothing -> putStrLn "⚠️  No portfolio had non-zero volatility."
            Just (bestSR, chosenNames, weights) -> do
              putStrLn "\n✔️  Best Anual Sharpe portfolio found:"
              putStrLn $ "    Sharpe Ratio : " ++ show bestSR
              putStrLn   "    Assets / Weights:"
              forM_ (zip chosenNames (U.toList weights)) $ \(nm, w) ->
                putStrLn $ "      " ++ nm ++ "  ->  " ++ show w
              putStrLn $ "\n⏱️  Elapsed time: " ++ showFFloat 2 elapsed " s"

              --------------------------------------------------------
              -- Evaluate on result dataset -------------------------
              --------------------------------------------------------
              putStrLn "\n🔁 Evaluating same portfolio on result dataset..."
              resData <- runExceptT (loadStockData resultPath)
              case resData of
                Left err2 -> putStrLn ("❌ Failed to load result CSV: " ++ err2)
                Right (names', priceRows') -> do
                  let nameToIdx nm = maybe (-1) id (lookup nm (zip names' [0 ..]))
                      idxs         = map nameToIdx chosenNames
                  if any (< 0) idxs
                    then putStrLn "⚠️  Some selected stocks not found in result CSV."
                    else do
                      let priceMatrix'  = toPriceMatrix priceRows'
                          returnMatrix' = priceMatrixToReturns priceMatrix'
                          mu'           = muVector returnMatrix'
                          sigma'        = covarianceMatrix returnMatrix'
                          muSub         = selectByIndexesU idxs mu'
                          sigmaSub      = V.map (selectByIndexesU idxs) $
                                          V.backpermute sigma' (V.fromList idxs)
                      case sharpeRatioFast muSub sigmaSub weights of
                        Nothing    -> putStrLn "⚠️  New Anual harpe Ratio invalid (zero σ?)."
                        Just newSR -> do
                          putStrLn $ "✅ New Anual Sharpe Ratio: " ++ show newSR
                          if newSR > bestSR
                            then putStrLn "🥳 Improved Sharpe on result dataset!"
                            else putStrLn "😕 Sharpe did not improve."
