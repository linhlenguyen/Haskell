module CountN()
where
  -- countNL '2' $ map show [1..25]
  -- 9

  countN :: Char -> [Char] -> Int
  countN c ls = foldl (\x y -> if y == c then x + 1 else x) 0 ls

  countNL :: Char -> [[Char]] -> Int
  countNL c ls = foldl (+) 0 $ map (countN c) ls
