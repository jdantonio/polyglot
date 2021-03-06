module Data.Recursion where
import Data.Maybe
import Data.Array (concatMap, filter, length, map, null, range)
import Data.Array.Unsafe (head, last, tail)
import Prelude.Unsafe (unsafeIndex)
import Control.MonadPlus (guard)
import Data.Foldable (any, foldl, foldr)
import Debug.Trace

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

containsNumber :: [Number] -> Number -> Boolean
containsNumber arr match = any (\item -> item == match) arr

hasFactors :: Number -> Boolean
hasFactors n = any (\pair -> (not (pair `containsNumber` 1)) && (not (pair `containsNumber` n))) (factors n) 

isPrime :: Number -> Boolean
isPrime n = length (factors n) == 1

--foreign import unsafeAt
--  "function unsafeAt (arr) {\
--  \  return function (i) {\
--  \    return arr[i];\
--  \  };\
--  \}" :: forall a. [a] -> Number -> a

cartesian :: forall a. [Number] -> [Number] -> [[Number]]
cartesian a b = do
  i <- (0..(length a)-1)
  j <- (0..(length b)-1)
  return [(unsafeIndex a i), (unsafeIndex b j)]

--reverse :: forall a. [a] -> [a]
--reverse [] = []
--reverse (x : xs) = reverse xs ++ [x]

--reverse :: forall a. [a] -> [a]
--reverse arr = reverse' [] arr
--  where
--    reverse' acc [] = acc
--    reverse' acc (x : xs) = reverse' (x : acc) xs

reverse :: forall a. [a] -> [a]
--reverse arr = foldr (\item acc -> acc ++ [item]) [] arr
--reverse = foldr (\item acc -> acc ++ [item]) []
reverse = foldl (\acc item -> [item] ++ acc) []

allTrue :: forall a. [Boolean] -> Boolean
allTrue = foldl (\item acc -> item && acc) true
--allTrue arr = foldl (\item acc -> item && acc) true arr

count :: forall a. (a -> Boolean) -> [a] -> Number
count _ [] = 0
count p (x : xs) = if p x then 1 + count p xs else count p xs

main = do
  let array = (1..10)
  --print $ size array
  --print $ fact 10
  --print $ fib 10
  --print $ square array
  --print $ positives (-5..5)
  --print $ pairs 3
  --print $ pairs 10
  --print $ factors 3
  --print $ factors 10
  --print $ isPrime 13
  --print $ isPrime 14
  --print $ unsafeAt [1,2,3] 1
  --print $ cartesian [1,2,3] [4,5,6]
  --print $ containsNumber [1,5] 5
  --print $ containsNumber [1,5] 3
  --print $ hasFactors 3
  --print $ hasFactors 10
  --print $ reverse array
  --print $ allTrue [true, true, true]
  --print $ allTrue [true, false, true]
  print $ count (\item -> item) [true, false, true]
