import System.IO
import Data.List

type CoinType = Int
type ReturnValue = Int

parseInput :: String -> [String]
parseInput xs = reverse $ map reverse $ foldl foldingFnc [[]] xs
  where foldingFnc :: [String] -> Char -> [String]
        foldingFnc ls c = if any (\k -> k == c) ['0','1','2','3','4','5','6','7','8','9']
                            then if (null ls) then [[c]]
                              else (c : head ls) : (tail ls)
                            else if (null ls || null (head ls)) then ls else [] : ls

processInput :: [String] -> ([CoinType], ReturnValue)
processInput [] = ([],0)
processInput ls = let intList = map read $ ls in
                   (reverse $ sort $ init intList, last intList)

calculateReturnCoins :: ([CoinType], ReturnValue) -> [(CoinType,Int)]
calculateReturnCoins (_,0) = []
calculateReturnCoins ([],r) = error $ "Can't find exact change, left over amount is " ++ (show r)
calculateReturnCoins ((x:xs),r) = if r >= x then (x, div r x) : calculateReturnCoins (xs,(mod r x))
                                else calculateReturnCoins (xs,r)

processOutput :: [(CoinType, Int)] -> String
processOutput [] = "Nothing"
processOutput [(c,q)] = show c ++ "x" ++ show q
processOutput ((c,q):xs) = show c ++ "x" ++ show q ++ "," ++ processOutput xs

main :: IO ()
main = do
  eof <- isEOF
  if not eof
    then do
      inpStr <- getLine
      putStrLn $ processOutput $ calculateReturnCoins $ processInput $ filter (\x -> not $ null x) $ parseInput inpStr
      main
    else putStr ""
