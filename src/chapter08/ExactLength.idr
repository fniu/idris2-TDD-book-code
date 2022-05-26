

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a


exactLength : {m : _} -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)

exactLength {m} len input = case m == len of
                                 False => Nothing
                                 True => Just ?exactLength_rhs2
