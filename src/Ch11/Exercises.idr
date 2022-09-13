import Data.Bits
import Data.Primitives.Views
import Data.String

-- 11.1

-- 1

every_other : Stream int -> Stream int
every_other (x :: (x' :: xs)) = x' :: (every_other xs)

-- 2

data InfList : Type -> Type where
  (::) : (value : elem') -> Inf (InfList elem') -> InfList elem'
  
%name InfList xs, ys, zs


countFrom : Integer -> InfList Integer
countFrom x = x :: Delay (countFrom (x+1))

getPrefix : (count : Nat) -> InfList a -> List a
getPrefix Z xs = []
getPrefix (S k) (value :: xs) = value :: getPrefix k xs
  
labelWith : InfList Integer -> List a -> List (Integer, a)
labelWith (lbl :: lbls) [] = []
labelWith (lbl :: lbls) (val :: vals) = (lbl, val) :: labelWith lbls vals
  
label : List a -> List (Integer, a)
label = labelWith (countFrom 0)


Functor InfList where
  map func (x::xs) = func x :: map func xs

-- 3

data Face = Heads | Tails

getFace : Int -> Face

bound : Int -> Int

getFace seed = let x = bound seed in
               if x > 6 then Heads else Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips 0 _ = []
coinFlips (S k) (x :: xs) = getFace x :: (coinFlips k xs)
  

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'
bound x with (divides x 12)
  bound ((12 * div) + rem) | (DivBy div rem prf) = rem + 1

-- 4

square_root_approx : (number : Double) -> (approx: Double) -> Stream Double
  
square_root_approx number approx = let app' = (approx + (number / approx)) / 2 in
                                   app' :: square_root_approx number app'

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) ->
                    (approxs : Stream Double) -> Double
  
square_root_bound 0 number bound (x :: y) = x
square_root_bound (S k) number bound (x :: y) = if abs (x * x - number) <= bound then x
                                                else square_root_bound k number bound y
  
square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.000000000001 (square_root_approx number number)

-- 11.2

data InfIO : Type where
  Do : IO a -> (a -> Inf InfIO) -> InfIO
  Seq : IO () -> Inf InfIO -> InfIO
  
(>>=) : IO a -> (a -> Inf InfIO) -> InfIO
(>>=) = Do

(>>) :  IO () -> Inf InfIO -> InfIO
(>>) = Seq

data Fuel = Dry | More (Lazy Fuel)

run : Fuel -> InfIO -> IO()
run Dry _ = putStrLn "Out of fuel"
run (More x) (Do y f) = do res <- y
                           run x (f res)
run (More fuel) (Seq c d) = do c; run fuel d                           
partial
forever : Fuel
forever = More forever

totalREPL : (prompt : String) -> (action : String -> String) -> InfIO
totalREPL prompt action = do
  putStr prompt
  s <- getLine
  putStr $ action s
  totalREPL prompt action


-- 113.3 TODO
