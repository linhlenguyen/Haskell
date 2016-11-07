{-# LANGUAGE GADTs #-}

module IPCExp(
IPCExp(..)
)
where
  import Data.Data
  import Data.IPCRule

  data IPCExp a where
  Compound :: [IPCRule] -> IPCExp [IPCRule]
  
