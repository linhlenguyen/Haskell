
data IPCRule = IPCRule { rule_id :: Int
						 --organism_id :: Int,
						 --rule_type :: Int, 
						 --parameter_name :: String,
						 --parameter_value :: String,
						 --template_id :: Int,
						 --template_input :: String
						 } deriving (Show)

data IPCRules = Empty | NewIPCRule IPCRule [IPCRules] deriving (Show)

--let rule1 = IPCRule { rule_id = 1 }
--let rule2 = IPCRule { rule_id = 2 }
--let rule3 = IPCRule { rule_id = 3 }
--let rules = NewIPCRule rule1 [NewIPCRule rule2 [Empty], NewIPCRule rule3 [Empty], Empty]

--findRule NewIPCRule rule (x:xs) = 

--Binary tree 
--data Element = Element {

data BTree = Empty | BTree String BTree BTree

treeSearch x Empty = False
treeSearch x (BTree s lhs rhs) = x == s || treeSearch x lhs || treeSearch x rhs

--Count number of element in a tree
treeCount x Empty = 0
treeCount x (BTree s lhs rhs) 
| x == s = 1 + treeCount x lhs + treeCount x rhs
| otherwise = treeCount x lhs + treeCount x rhs

--Find first element in the tree
treeDetect x Empty = error "No element found"
treeDetect x (BTree s lhs rhs) 
| x == s = s
| otherwise = treeDetect x lhs 

treeRemove

treeAdd (BTree s1 l1 r1) (BTree s2 l2 r2)

