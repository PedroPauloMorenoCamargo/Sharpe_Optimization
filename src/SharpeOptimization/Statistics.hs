module SharpeOptimization.Statistics( 
    combinations,
    matrix_line_multiply,
    matrix_multiplied_by_vector,
    vector_multiplied_by_matrix,
    mean,
    selectByIndices,
    transpose
  ) where


-- Given a list of elements and an integer k, this function returns all possible combinations of k elements from the list.
combinations :: Int -> [a] -> [[a]]
-- if k == 0, return a list with an empty list
combinations 0 _      = [[]]
-- if the list is empty return an empty list
combinations _ []     = []
-- if k is greater than the length of the list, return an empty list
combinations k xs | k > length xs = []
-- if k == 1, return a list of lists with each element of the list
combinations 1 xs = map (:[]) xs
-- if k > 1, return all combinations of k-1 elements from the tail of the list and prepend the head of the list to each combination
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs


matrix_line_multiply :: [Double] -> [Double] -> Double
matrix_line_multiply [] _ = 0
matrix_line_multiply _ [] = 0
matrix_line_multiply (x:xs) (y:ys) = x * y + matrix_line_multiply xs ys

matrix_multiplied_by_vector :: [[Double]] -> [Double] -> [Double]
matrix_multiplied_by_vector [] _ = []
matrix_multiplied_by_vector _ [] = []
matrix_multiplied_by_vector (x:xs) v = matrix_line_multiply x v : matrix_multiplied_by_vector xs v


mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

selectByIndices :: [Int] -> [a] -> [a]
selectByIndices idxs xs = map (xs !!) idxs


transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)
