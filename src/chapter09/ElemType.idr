import Data.Vect
import Data.Vect.Elem
import Decidable.Equality

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There later) impossible

notInTail : (notHere: value = x -> Void) -> (notThere : Elem value xs -> Void) -> Elem value (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs) 
isElem value [] = No notInNil
isElem value (x :: xs) = case decEq value x of
                              Yes Refl => Yes Here
                              No contra => case Main.isElem value xs of
                                                (Yes prf) => Yes (There prf)
                                                (No f) => No (notInTail contra f)
