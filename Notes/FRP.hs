--FRP
--Simpliest example : excel, expression of dependencies
--Is KVO reactive?
--
--Events/Streams (anything can be streams)
--Axis of time/ discreet currency over time of certain events
--E.g Mouse clicks, 
--Change event text field. 
--Event streams of key press filter (\keypress -> keypress.key = 'Enter') keyPressEvents
--filter, map, merge
--
--Behaviours/Signals <- lift
--Always has value over continuous time (windows, input field)
--
--Glitch free propagation
--

--Some random type declaration
data TypeA a = TypeA { getA :: a, runInt :: Int -> a, runBool :: Bool -> a }

let a = TypeA { getA = 5, runInt = \x -> x + 5, runBool = \x -> if x == True then 5 else 4 }
let b = a { runBool = \x -> 10 }


--Behaviour always has value over continuous time!
newtype Behaviour a = Behaviour { at :: Time -> a }

myName :: Behaviour Text
myName 'at' '27-05-2016T12:14:00'

myEditWidget :: Behaviour Text
liftA2 (<>) myEdit1 myEdit2




