import Data.List

data SnocList : List a -> Type where
  Empty : SnocList []
  Snoc : (x, xs : _) -> (rec : SnocList xs) -> SnocList (xs ++ [x])
  
snocListHelp : {input : _} ->
               (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
               
snocListHelp snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp snoc (x :: xs)
  = rewrite appendAssociative input [x] xs in
            snocListHelp (Snoc x input snoc) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] Empty = []
myReverseHelper (xs ++ [x]) (Snoc x xs rec) = x :: myReverseHelper xs rec

myReverse : List a -> List a
myReverse input with (snocList input) 
  myReverse [] | Empty = []
  myReverse (xs ++ [x]) | (Snoc x xs rec) = x :: myReverse xs | rec

my_reverse : List a -> List a
my_reverse input with (snocList input)
  my_reverse [] | Empty = []
  my_reverse (xs ++ [x]) | (Snoc x xs rec) = x :: my_reverse xs | rec
