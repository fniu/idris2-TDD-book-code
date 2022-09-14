import Control.Monad.State
 
-- 12.1 
-- 1 
update : (stateType -> stateType) -> State stateType ()
update f = do x <- get
              put $ f x

increment : Nat -> State Nat ()
increment x = update (+x)

-- 2

data Tree a = Empty
            | Node (Tree a) a (Tree a)

testTree : Tree String
testTree = Node (Node (Node Empty "Jim" Empty) "Fred"
                      (Node Empty "Sheila" Empty)) "Alice"
                (Node Empty "Bob" (Node Empty "Eve" Empty))
  
countEmpty : Tree a -> State Nat ()
countEmpty Empty = put 1
countEmpty (Node Empty y Empty) = do x <- get
                                     put (x+2)
countEmpty (Node left y Empty) = do x <- get
                                    put (x+1)
                                    countEmpty left
countEmpty (Node Empty y right) = do x <- get
                                     put (x+1)
                                     countEmpty right
countEmpty (Node left y right) = do countEmpty left
                                    countEmpty right
