{-# LANGUAGE  QuasiQuotes  #-}

module UseQuotes where

import RegExp
import Language.Haskell.TH.Quote


r9 = [r| x y* (a+b) |]
r10 = [r| w* $r9 |]
r11 = [r| z + @(map One "abc") |]

isCat ([r| $x $y |]) = True
isCat x = False

isStar ([r| $x * |]) = True
isStar x = False

f ([r| w * $(One y) |]) = y
f _ = 'z'

r12 = [r| w*q |]
 
