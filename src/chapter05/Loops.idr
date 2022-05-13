module Main

import System
import Data.String

countdown : (secs : Nat) -> IO ()
countdown 0 = putStrLn "Lift off!"
countdown (S k) = do putStrLn (show (S k))
                     usleep 1000000
                     countdown k

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (stringToNatOrZ input))
    else pure Nothing


countdowns : IO ()
countdowns = do putStr "Enter starting number: "
                Just startNum <- readNumber
                  | Nothing => do putStrLn "Invalid input"
                                  countdowns
                countdown startNum
                putStr "Another (y/n)? "
                yn <- getLine
                if yn == "y" then countdowns
                             else pure ()
