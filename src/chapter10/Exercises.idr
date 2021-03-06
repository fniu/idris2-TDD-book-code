

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
