import Data.Vect

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y



maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe a Nothing = a
maxMaybe Nothing a = a
maxMaybe (Just x) (Just y) = case x > y of
                                  False => Just y 
                                  True => Just x

vectTake : (n : Nat) -> Vect (n+m) a -> Vect n a
vectTake 0 xs = Vect.Nil
vectTake (S n) (x :: xs) = x :: (vectTake n xs)


sumEntries : Num a => {n:_}->(pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                            Nothing => Nothing
                            (Just idx) => Just ((index idx xs) + (index idx ys))
