import Data.Nat
import Data.Vect

same_cons : {xs : List a} -> {ys : List a} ->
            xs = ys -> x :: xs = x :: ys
--same_cons {x} prf = cong (\xs => x :: xs) prf
same_cons Refl = Refl

same_lists : {xs : List a} -> {ys : List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
             
same_lists Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  ThreeSame : (x : a) -> ThreeEq x x x

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x x x (ThreeSame x) = ThreeSame (S x)

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes 0 m = sym (plusZeroRightNeutral m)
myPlusCommutes (S k) m = rewrite (myPlusCommutes k m) in (plusSuccRightSucc m k) 

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect k a -> Vect m a -> Vect (k+m) a
        reverse' acc [] = rewrite plusZeroRightNeutral k in acc
        reverse' {m = S len} acc (x :: xs)
                        = rewrite sym (plusSuccRightSucc k len) in
                                  (reverse' (x :: acc) xs)
