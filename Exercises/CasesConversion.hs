{--
isString : camel
is_string : snake

Write a function that converts a camel case string to snake case:

input: likeThis
output: like_this
--}

import Data.Map
import Data.Char
import Data.List

isCapital :: Char -> Bool
isCapital c = let r = ord c in if r >= 65 && r <= 90 then True else False

--isHTTP -> is_http -> [is, HTTP] ->

--isIBMCool -> is_ibm_cool -> [is, IBMC, ool]
--isIBMCoolYESOrNO -> [is, IBMC, ool, YESO, r, NO]
--[isString] -> [is, S, tring]

hasChange :: Char -> Char -> Bool
hasChange c1 c2 = isLower c1 && isCapital c2 || isCapital c1 && isLower c2

splitS :: String -> [String]
splitS (s:ss) = reverse $ Prelude.map (reverse) $
                Prelude.foldl (\a@(x:xs) c -> if hasChange c (head x) then
                                                  if isLower c then
                                                    if Prelude.null (tail x) then ((c:[head x]):xs)
                                                    else ((c:[head x]):(tail x):xs)
                                                  else ([c]:a)
                                              else ((c:x):xs)) [[s]] ss

toSnake :: String -> String
toSnake [] = []
toSnake ls = concatMap (Prelude.map toLower) $ intersperse "_" $ splitS $ ls


{--
toWords :: String -> [String]
toWords str = foldl foldf [] str
    where foldf :: [String] -> Char -> String
          foldf (x:xs) c -> if
--}

--state machine
--l > l : ignore
--l -> h : l_lower(h)
--h1 -> h2h3: lower(h1)_lower(h2)
--h1 -> h2l1: lower(h1)lower(h2)
--h2 -> null: h2
----------------
