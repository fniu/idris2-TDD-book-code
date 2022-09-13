
append : List a -> List a -> List a
append [] ys = ys
append (x :: xs) ys = x :: append xs ys
