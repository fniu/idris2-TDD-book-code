my_length : List a -> Nat

my_length [] = 0
my_length (x :: xs) = S (my_length xs)

myreverse : List a -> List a
  
myreverse [] = []
myreverse (x :: xs) = (myreverse xs) ++ [x]

mymap : (a -> b) -> List a -> List b
mymap f [] = []
mymap f (x :: xs) = f x :: mymap f xs
