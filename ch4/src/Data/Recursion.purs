module Data.Recursion where
import Data.Maybe
import Data.Array (null)
import Data.Array.Unsafe (head, tail)
import Debug.Trace

length :: forall a. [a] -> Number
length arr =
  if null arr
     then 0
     else 1 + length (tail arr)

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
isEven n =
  if n % 2 == 0
     then true
     else false

countEven :: forall a. [a] -> Number
countEven [] = 0
countEven arr =
  --if 1 == head arr
  if 1 == head [100]
     then 1
     else 2

main = print (countEven [1,2,3,4,5,6,7,8,9,10])
--main = print (countEven [])
