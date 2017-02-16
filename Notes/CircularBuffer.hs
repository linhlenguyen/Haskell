module CircularBuffer(

)
  where
    type Head = Int
    type Tail = Int
    type Size = Int
    type Buffer a = (Head, Tail, Size, [a])

    newIntBuffer :: Buffer Int
    newIntBuffer = (0, 9, 10, [])

    movePointer :: Int -> Size -> Int
    movePointer i s = mod (i+1) s

    writeBuffer :: Int -> Buffer a -> Buffer a
    writeBuffer i b@(h, t, s, xs) = if (h != t) then (movePointer h s, t, s, t:xs) else b

    readBuffer :: Buffer a -> (a, Buffer a)
    readBuffer b@(h, t, s, xs) = undefined
