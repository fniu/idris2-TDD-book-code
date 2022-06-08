
import Data.Vect
import Data.Vect.Elem
import Decidable.Equality

removeElem : {n : _} ->
             (value : a) -> (xs : Vect (S n) a) -> 
             {auto prf : Elem value xs} ->
             Vect n a
removeElem value (value :: xs) {prf = Here} = xs
removeElem {n = 0} value (x :: []) {prf = There later} = absurd later
removeElem {n = (S k)} value (x :: xs) {prf = There later} = x :: removeElem value xs

maryInVector : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVector = There (There Here)
