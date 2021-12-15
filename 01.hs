l = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

-- | Count number of increases in a list of numbers
count_inc :: (Num a, Ord a) => [a] -> a
count_inc (x:x':xs) | x < x' = count_inc (x':xs) + 1
count_inc (x:x':xs) = count_inc (x':xs) + 0
count_inc _ = 0

-- | Count number of increases with three-measurement window
count_inc' :: (Num a, Ord a) => [a] -> a
count_inc' (a:b:c:d:xs) | (a + b + c) < (b + c + d) = count_inc' (b:c:d:xs) + 1
count_inc' (a:b:c:d:xs) = count_inc' (b:c:d:xs) + 0
count_inc' _ = 0

main :: IO()
main = do
  s <- readFile "01_input.txt"
  let ints = map read (lines s) :: [Integer]
  print $ count_inc ints
  print $ count_inc' ints
