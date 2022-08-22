

data InfIO : Type where
  Do : IO a
       -> (a -> Inf InfIO)
       -> InfIO
       
loopPrint : String -> InfIO
loopPrint msg = Do (putStrLn msg)
                   (\_ => loopPrint msg)


data Fuel = Dry | More Fuel

tank : Nat -> Fuel
tank Z = Dry
tank (S k) = More (tank k)

run : Fuel -> InfIO -> IO()

run Dry y = putStrLn "Out of fuel"
run (More x) (Do y f) = do res <- y
                           run x (f res)

