
import Data.Vect

append : Vect n elem' -> Vect m elem' -> Vect (n + m) elem'

append [] ys = ?append_rhs_1
append (x :: xs) ys = ?append_rhs_2
