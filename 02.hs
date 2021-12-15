
-- s = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2\n"

-- | parse a single line of input into a tuple of (direction, amount)
parseLine :: String -> (String, Int)
parseLine line =
    let (direction:amountStr:[]) = words line
    in (direction, read amountStr :: Int)

-- | Add a line of direction output to existing position
addLine :: (Int, Int) -> String -> (Int, Int)
addLine (horizontal, depth) line =
    let (direction, amount) = parseLine line
    in case direction of
        "forward" -> (horizontal + amount, depth)
        "down" -> (horizontal, depth + amount)
        "up" -> (horizontal, depth - amount)

-- | Add a line of direction output to existing position (with aim)
addLine' :: (Int, Int, Int) -> String -> (Int, Int, Int)
addLine' (horizontal, depth, aim) line =
    let (direction, amount) = parseLine line
    in case direction of
        "forward" -> (horizontal + amount, depth + aim * amount, aim)
        "down" -> (horizontal, depth, aim + amount)
        "up" -> (horizontal, depth, aim - amount)


main :: IO()
main = do
  s <- readFile "02_input.txt"
  let inputLines = lines s
  let (horizontal, depth) = foldl addLine (0, 0) inputLines
  print $ horizontal * depth
  let (horizontal', depth', _) = foldl addLine' (0, 0, 0) inputLines
  print $ horizontal' * depth'
