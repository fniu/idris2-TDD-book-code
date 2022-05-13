import Data.String
import Data.Nat
import Data.Vect
import System.File

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (stringToNatOrZ input))
    else pure Nothing

  
oneGuess : (minB: Nat) -> (maxB: Nat) -> (target: Nat) -> IO ()
oneGuess minB maxB target = do
  putStrLn $ "Make a guess between " ++ show minB ++ " and " ++ (show maxB) ++ ": " 
  Just numb <- readNumber
    | Nothing => do putStrLn "Must type a number!"
                    oneGuess minB maxB target
  case numb == target of
    True => do putStrLn "Correct answer!"
    False => do
      let msg = if numb > target then "Too high" else "Too low"
      putStrLn msg
      oneGuess minB maxB target

-- randomTarget : IO Nat
-- randomTarget = do
--   t <- System.time
--   let n = fromInteger t
--   let m = modNat n 100
--   pure m

guess : IO ()
guess = do
  putStrLn "Guess game"
  let maxBound = 100
  let minBound = 0
  let target = 30
  oneGuess minBound maxBound target
  putStrLn "Done"


readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "")
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)

readAndSave : IO ()
readAndSave = do
  s <- readToBlank
  putStrLn "Enter a file name:"
  fn <- getLine
  Right e <- writeFile fn (unlines s) 
    | Left err => putStrLn (show err)
  putStrLn "File written."
  

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
  Right s <- readFile filename
    | Left err => do putStrLn (show err)
                     pure (_ ** Nil)
  let ls = lines s
  let ln = length ls
  case toVect ln ls of
    Just vec' => do putStrLn $ "Vect read: " ++ (show vec')
                    pure (_ ** vec') 
    Nothing => pure (_ ** Nil)
  
  

