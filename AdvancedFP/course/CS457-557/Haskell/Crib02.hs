module CribBasicTypes where
import Char

i0:: Integer
i0  = 23          -- 
i1  = 23 + 1      --  24
i2  = 23 - 1      --  22
i3  = 23 * 1      --  23
i4  = 23 `div` 3  --  7
i5  = 23 `mod` 3  --  2
i6  = 23 <  3     --  False
i7  = 23 <= 3     --  False 
i8  = 23 >  3     --  True
i9  = 23 >= 3     --  True
i10 = 23 /= 3     --  True
i11 = 23 == 3     --  False


d0:: Double
d0  = 6.2             --
d1  = 6.2 + 1.0       --  7.2
d2  = 6.2 - 1.0       --  5.2
d3  = 6.2 * 1.0       --  6.2
d4  = 6.2 / 1.0       --  6.2

d6  = 6.2 <  3.1      --  False
d7  = 6.2 <= 3.1      --  False
d8  = 6.2 >  3.1      --  True
d9  = 6.2 >= 3.1      --  True
d10 = 6.2 /= 3.1      --  True   

d11:: Double
d11 = fromIntegral 11 -- 11.0

d12,d13,d14:: Integer
d12 = round 6.7       -- 7
d13 = floor 6.7       -- 6
d14 = ceiling 6.7     -- 7


c0:: Char
c0 = 'a'
c1 = toUpper 'm'     -- 'M'
c2 = toLower 'D'     -- 'd'
c3 = digitToInt '4'  -- 4
c4 = intToDigit 7    -- '7'
c5 = ord 'b'         -- 98
c6 = chr 99          -- 'c'
c7 = "abc" == 
     ['a','b','c']   -- True
     

s0:: String
s0 = "abc"
s1:: Integer
s1 = read "23"            -- 23
s2 = (read "True")::Bool  -- True
s3 = "John "++ "Smith"    -- "John Smith"
s4 = 'x':"abc"            -- "xabc"
s5 = ['h','i'] == "hi"    -- True


-- Lists
l0 = [1,2,3,4,5] 
l1 = l0 ++ l0        -- [1,2,3,4,5,1,2,3,4,5]
l2 = 2 : [5,9]       -- [2,5,9]
l3 = reverse l0      -- [5,4,3,2,1]
l4 = length l0       -- 5
l5 = drop 2 "abcde"  -- "cde"
l6 = take 2 "abcde"  -- "ab"
l7 = head [1,2,3,4]  -- 1
l8 = tail [1,2,3,4]  -- [2,3,4]
l9 = sum [1,2,3,4,5] -- 1+2+3+4+5 = 15
l10= concat [[1],[1,3,5],[]]    -- [1,1,3,5]


-- Tuples
t0 = (2,"abc")
t1 = fst t0     -- 2
t2 = snd t0     -- "abc"
t3 = (3,True,'z')
t4 = (3,'c',"a",2.0)