--   Module responsible for loading a CSV file containing stock price data.
--   Returns the stock names (excluding the date column)
--   and a matrix of prices as Doubles.
module SharpeOptimization.DataLoader (loadStockData) where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, IOException)
import Control.Monad (when)
import Data.List.Split (splitOn)

--   Loads stock data from a CSV file with header "Date,AAPL,AMZN,...".
--   Returns a list of stock names and a matrix of Doubles (daily x prices).
--   Uses `ExceptT String IO` to handle:
--     - File I/O errors
--     - Empty file
--     - Invalid headers
--     - Malformed or non-numeric values
loadStockData :: FilePath -> ExceptT String IO ([String], [[Double]])
loadStockData path = do
  content <- readFileSafe path
  parseCSV content

--   Reads a file safely using exception handling.
--   Returns the file contents as a String or throws a descriptive error if reading fails.
readFileSafe :: FilePath -> ExceptT String IO String
readFileSafe path = do
  result <- liftIO $ try (readFile path) :: ExceptT String IO (Either IOException String)
  case result of
    Left e  -> throwE $ "Error reading input file: " ++ show e
    Right t -> return t

--   Parses CSV content assuming the first column is a date and the remaining columns are numeric stock prices.
--   Returns the stock names and a matrix of Doubles (daily x prices).
--   Validates:
--     - At least two columns in header
--     - Consistent number of columns in data rows
--     - Proper numeric values
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
        else throwE "Inconsistent number of columns in data rows"

--   Parses a single line of CSV (excluding the date column).
--   Converts each value to Double, or fails if any value is invalid.
parseLine :: String -> ExceptT String IO [Double]
parseLine line = traverse parseDouble (tail (splitOn "," line))

--   Parses a string into a Double.
--   Returns an error message if parsing fails.
parseDouble :: String -> ExceptT String IO Double
parseDouble s =
  case reads s of
    [(x, "")] -> return x
    _         -> throwE $ "Invalid numeric value: " ++ s
