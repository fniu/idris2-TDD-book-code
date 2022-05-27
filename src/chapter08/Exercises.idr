

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
