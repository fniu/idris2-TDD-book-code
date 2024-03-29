import Data.List.Views

data TakeN : List a -> Type where
     Fewer : TakeN xs
     Exact : (n_xs : List a) -> {rest : _} -> TakeN (n_xs ++ rest)
     
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN 0 _ = Exact []
takeN _ [] = Fewer
takeN (S k) (y :: ys) = case takeN k ys of
                             Fewer => Fewer
                             Exact n_xs => Exact (y::n_xs)

groupByN : (n : Nat) -> (xs : List a) -> List (List a)
groupByN n xs with (takeN n xs)
  groupByN n xs | Fewer = [xs]
  groupByN n (n_xs ++ rest) | (Exact n_xs) = n_xs :: groupByN n rest


halves : List a -> (List a, List a)
halves xs with (takeN (cast ((cast (length xs)) `div` 2)) xs)
  halves xs | Fewer = ([], xs)
  halves (ys ++ zs) | Exact ys = (ys, zs)

--- 10.2 ---
-- 1
equalSuffix : Eq a => List a -> List a -> List a
equalSuffix lst1 lst2 with (snocList lst1, snocList lst2)
  equalSuffix [] _ | (_, _) = []
  equalSuffix _ [] | (_, _) = []
  equalSuffix _ _ | (Snoc x xs xr, Snoc y ys yr) =
    if x == y then (equalSuffix _ _ | (xr, yr)) ++ [x] else []
  
-- TODO: 2

-- TODO: 3

-- TODO: 4
