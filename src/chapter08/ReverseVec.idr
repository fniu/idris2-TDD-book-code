
import Data.Vect
import Data.Nat

myReverse : Vect n elem' -> Vect n elem'
myReverse [] = []
myReverse {n = S k} (x :: xs) = let result = myReverse xs ++ [x] in
                          rewrite plusCommutative 1 k in
                          result
                          
--myReverse (x :: xs) = let result = myReverse xs ++ [x] in
--                          ?myReverse_rhs
