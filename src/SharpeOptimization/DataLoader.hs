module SharpeOptimization.DataLoader (loadStockData) where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, IOException)
import Control.Monad (when)
import Data.List.Split (splitOn)

loadStockData :: FilePath -> ExceptT String IO ([String], [[Double]])
loadStockData path = do
  content <- readFileSafe path
  parseCSV content

readFileSafe :: FilePath -> ExceptT String IO String
readFileSafe path = do
  result <- liftIO $ try (readFile path) :: ExceptT String IO (Either IOException String)
  case result of
    Left e  -> throwE $ "Error Reading Input File: " ++ show e
    Right t -> return t

parseCSV :: String -> ExceptT String IO ([String], [[Double]])
parseCSV input =
  case lines input of
    [] -> throwE "Empty file"
    (header:rows) -> do
      let cols = splitOn "," header
      when (length cols < 2) $
        throwE "Header must have at least 2 columns"
      let names = tail cols
      parsed <- traverse parseLine rows
      let expected = length names
      if all (\row -> length row == expected) parsed
        then return (names, parsed)
        else throwE "Invalid number of columns in data rows"

parseLine :: String -> ExceptT String IO [Double]
parseLine line = traverse parseDouble (tail (splitOn "," line))

parseDouble :: String -> ExceptT String IO Double
parseDouble s =
  case reads s of
    [(x, "")] -> return x
    _         -> throwE $ "Invalid Value: " ++ s
