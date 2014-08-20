module Data.Recursion where
import Data.Array (null)
import Data.Array.Unsafe (head, tail)

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

isEven :: Number -> Boolean
isEven n =
  if n % 2 == 0
     then true
     else false

--countEven :: forall a. [a] -> Number
--countEven arr =
  --if null arr
     --then 0
     --else if isEven (head arr)
     --then 1 + countEven (tail arr)
     --else countEven (tail arr)
