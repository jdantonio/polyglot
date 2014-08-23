module Data.Recursion where
import Data.Maybe
import Data.Array (concatMap, filter, length, map, null, range)
import Data.Array.Unsafe (head, last, tail)
import Control.MonadPlus (guard)
import Debug.Trace

-- re-implement the `length` function as `size`
size :: forall a. [a] -> Number
size arr =
  if null arr
     then 0
     else 1 + size (tail arr)

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
isEven 0 = true
isEven 1 = false
isEven n = isEven (n % 2)

countEven :: forall a. [Number] -> Number
countEven [] = 0
countEven arr =
  if isEven $ head arr
     then 1 + (countEven $ tail arr)
     else 0 + (countEven $ tail arr)

square :: forall a. [Number] -> [Number]
square arr = map (\n -> n * n) arr

infix 1 <$?>

(<$?>) fn arr = filter fn arr

positives :: forall a. [Number] -> [Number]
positives arr = (\n -> n >= 0) <$?> arr

product :: forall a. [Number] -> Number
product pair = (head pair) * (last pair) 

pairs :: Number -> [[Number]]
pairs n = concatMap (\i -> map (\j -> [i, j]) (i..n)) (1..n)

--factors :: Number -> [[Number]]
--factors n = filter (\pair -> product pair == n) (pairs n)

factors :: Number -> [[Number]]
factors n = do
  i <- (1..n)
  j <- (i..n)
  guard $ i * j == n
  return [i, j]

isPrime :: Number -> Boolean
isPrime n = length (factors n) == 1

main = do
  let array = (1..10)
  --print $ size array
  --print $ fact 10
  --print $ fib 10
  --print $ square array
  --print $ positives (-5..5)
  --print $ pairs 3
  --print $ factors 10
  print $ isPrime 13
  print $ isPrime 14
