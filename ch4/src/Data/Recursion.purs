module Data.Recursion where
import Data.Maybe
import Data.Array (concatMap, filter, map, null)
import Data.Array.Unsafe (head, last, tail)
import Debug.Trace

--length :: forall a. [a] -> Number
--length arr =
--  if null arr
--     then 0
--     else 1 + length (tail arr)

fact :: Number -> Number
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Number -> Number
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

(..) :: Number -> Number -> [Number]
(..) = Data.Array.range

isEven :: Number -> Boolean
-- -- with conditionals
--isEven n =
--  if n % 2 == 0
--     then true
--     else false
-- with pattern matching and recursion
isEven 0 = true
isEven 1 = false
isEven n = isEven (n % 2)

--countEven :: forall a. [a] -> Number
--countEven [] = 0
--countEven arr =
--  if isEven $ head [100]
--     then 1
--     else 2

square :: forall a. [Number] -> [Number]
square arr = map (\n -> n * n) arr

infix 1 <$?>

(<$?>) fn arr = filter fn arr

isPositive :: forall a. [Number] -> [Number]
isPositive arr = (\n -> n >= 0) <$?> arr

product :: forall a. [Number] -> Number
product pair = (head pair) * (last pair) 

pairs :: Number -> [[Number]]
pairs n = concatMap (\i -> map (\j -> [i, j]) (i..n)) (1..n)

factors :: Number -> [[Number]]
factors n = filter (\pair -> product pair == n) (pairs n)

main = print $ factors 10
