
import Data.Vect

elem : Eq ty => (value : ty) -> (xs : Vect n ty) -> Bool
elem value [] = False
elem value (x :: xs) = case value == x of
                            False => Main.elem value xs
                            True => True
