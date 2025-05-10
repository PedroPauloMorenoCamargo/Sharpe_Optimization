module Main where

import SharpeOptimization.Statistics

main :: IO ()
main = do
  putStrLn "Produto escalar:"
  print $ matrix_line_multiply [1,2,3] [4,5,6]  -- 32.0

  putStrLn "\nMatriz × vetor:"
  let mat1 = [[1,0,2], [0,3,-1]]
      vec1 = [4,5,6]
  print $ matrix_multiplied_by_vector mat1 vec1  -- [16.0, 9.0]

  putStrLn "\nVetor × matriz:"
  let vec2 = [1,2]
      mat2 = [[3,4,5], [6,7,8]]
  print $ matrix_multiplied_by_vector (transpose mat2) vec2  -- [15.0, 18.0, 21.0]
