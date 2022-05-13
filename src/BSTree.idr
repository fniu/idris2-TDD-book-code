
data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node : Ord elem => (left : BSTree elem) -> (val :elem) ->
                     (right : BSTree elem) -> BSTree elem

