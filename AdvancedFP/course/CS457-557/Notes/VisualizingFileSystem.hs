module VisualizingFileSystem where

import Treedot2
import IOActions

{-
data FileSystem = File FilePath
             | Folder FilePath [FileSystem]
             | Foldep FilePath
     deriving Show
-}

instance Tree FileSystem where
  subtrees (File p) = []
  subtrees (Folder p xs) = xs
  subtrees (Foldp p) = []
  

instance LabeledTree FileSystem where
  label (File p) = p
  label (Folder p xs) = p
  label (Foldp p) = p