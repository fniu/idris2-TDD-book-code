import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix 0 _ = Vect 0 Double
Matrix _ 0 = Vect 0 Double
Matrix n m = Vect n (Vect m Double)
