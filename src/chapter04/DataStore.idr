module Main

import Data.Vect
import System.REPL
import Data.String

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs


data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit
             
parseCommand : String -> String -> Maybe Command
parseCommand "add" y = Just (Add y)
parseCommand "search" y = Just (Search y)
parseCommand "get" y = case all isDigit (unpack y) of
                            False => Nothing
                            True => Just (Get (cast y))
parseCommand "quit" y = Just Quit
parseCommand "size" y = Just Size
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : Integer -> DataStore -> String -> Maybe (String, DataStore)
getEntry pos store inp = let store_items = items store in
                             case integerToFin pos (size store) of
                                  Nothing => Just ("Out of of range\n", store)
                                  Just id => Just (index id store_items ++ "\n", store)

lookupStoreAt : Integer -> DataStore -> String -> String
lookupStoreAt pos store w = let store_items = items store in
                                case integerToFin pos (size store) of
                                     Nothing => ""
                                     Just id =>
                                       let item = index id store_items
                                           matched = isInfixOf w item in
                                           case matched of
                                                True => (show id ++ ": " ++ item)
                                                False => ""
                                              
findAllEntries : DataStore -> String -> String
findAllEntries store w = let store_size = natToInteger (size store)
                             strs = map (\i => lookupStoreAt i store w) [0..store_size] in
                               unlines (filter (/= "") strs)

searchEntry : String -> DataStore -> Maybe (String, DataStore)
searchEntry "" store = Just ("\n", store)
searchEntry w store = Just (findAllEntries store w, store)

processInput : DataStore -> String -> Maybe (String, DataStore)
 
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              (Just (Add x)) => 
                                Just ("ID " ++ show (size store) ++ "\n", addToStore store x)
                              (Just (Get x)) => getEntry x store inp 
                              (Just Quit) => Nothing
                              (Just Size) => Just ("size: " ++ show (size store) ++ "\n", store)
                              (Just (Search w)) => searchEntry w store

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
