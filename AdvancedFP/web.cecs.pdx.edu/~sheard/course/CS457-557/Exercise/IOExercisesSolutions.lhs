-------------------

> import IOActions

-------------------
1) How many Haskell source files are there in the
current directory?

> problem1
>  = haskellFiles
>    >>= inIO length
>    >>= print

Looks like I'm going to need this more than once,
so I defined a reusable IOAction for getting the
list of all Haskell files in the current
directory:

> haskellFiles
>  = getCurrentDirectory
>    >>= getFiles
>    >>= inIO (filter (isSuffixOf "hs"))

-------------------
2) How many lines of Haskell source code are in
the current directory?

> problem2
>  = haskellFiles
>    >>= mapM fileLength
>    >>= inIO sum
>    >>= print

Again, I spotted a section of code that I'm going
to want again, so I have abstracted it out as
follows:

> fileLength f
>  = return f
>    >>= readFile
>    >>= inIO (length. lines)

-------------------
3) What is the largest Haskell source file in the
current directory

> problem3
>  = largestHaskellFile >>= print

Yet again, peeking ahead at the next part, I
realized that this would not be the last time I'd
want to find the largest Haskell file ...

> largestHaskellFile
>  = haskellFiles
>    >>= mapM (\f -> fileLength f >>= \l -> return (l, f))
>    >>= inIO (snd . maximum)

-------------------
4) Copy the largest Haskell source file in the
current directory into Largest.hs

I object, that wasn't a question!  But I know what
you mean ... :-)

> problem4
>  = largestHaskellFile
>    >>= readFile
>    >>= writeFile "zLargest.hs"

-------------------
