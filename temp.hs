fa 0 = 1
fa n = n * fa (n - 1)

nCr n r = div (fa n) (fa (n-r) * fa r)
 
