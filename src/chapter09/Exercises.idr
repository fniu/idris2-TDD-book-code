
import Data.List
import Decidable.Equality

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

notInNil : Last [] value -> Void
notInNil LastOne impossible

notInTail0 : (y = value -> Void) -> Last [y] value -> Void
notInTail0 f LastOne = f Refl

notInTail : (Last (y' :: ys') value -> Void) -> Last (y :: (y' :: ys')) value -> Void
notInTail f (LastCons prf) = f prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notInNil
isLast (y :: ys) value = case ys of
                              [] => case decEq y value of
                                         Yes Refl => Yes LastOne
                                         No contra => No (notInTail0 contra) 
                              (y' :: ys') => case isLast ys value of
                                                  (Yes prf) => Yes (LastCons prf)
                                                  (No contra) => No (notInTail contra)
