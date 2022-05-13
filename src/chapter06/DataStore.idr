module Main

import Data.Vect
import System.REPL
import Data.String

infixr 5 .+.
data Schema = SString
            | CChar
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType CChar = Char
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)


addToStore : (store: DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema' size store) newitem = MkData schema' _ (addToData store)
  where
    addToData : Vect old (SchemaType schema') -> Vect (S old) (SchemaType schema')
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

  
display : {schema : _} -> SchemaType schema -> String
display {schema = SString} item = show item
display {schema = CChar} item = show item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getEntry : Integer -> DataStore -> Maybe (String, DataStore)
getEntry pos store = 
  let store_items = items store in
  case integerToFin pos (size store) of
       Nothing => Just ("Out of of range\n", store)
       Just id => Just (display (index id store_items) ++ "\n", store)


getAllEntries : DataStore -> String
getAllEntries store = 
  let size_store = size store 
      store_items = items store in
  case size_store of
    0 => "Data store is empty.\n" 
    S k => case rangeFromTo 0 k of
       [] => "Error"
       (x :: xs) => foldl1By (\acc, i => acc ++ case getEntry (cast i) store of
                                                     Just (x, _) => show i ++ ": " ++ x
                                                     _ => "Not found"
                      )
                      (\i => case getEntry (cast i) store of
                                  Just (x, _) => show i ++ ": " ++ x
                                  _ => "Not found"
                      )
                      (x :: xs)
       
data Command : Schema -> Type where
  SetSchema : (newschema: Schema) -> Command schema'
  Add: SchemaType schema' -> Command schema'
  Get: Integer -> Command schema'
  Quit: Command schema'


parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs)
      = case span (/= '"') xs of
             (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
             _ => Nothing
    getQuoted _ = Nothing
    
parsePrefix CChar input = case unpack input of
                               [] => Nothing
                               ch :: (' ' :: rest) => Just (ch, ltrim (pack rest))
                               _ => Nothing

parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
                              
parsePrefix (schemal .+. schemar) input =
  do (l_val, input') <- parsePrefix schemal input
     (r_val, input'') <- parsePrefix schemar input'
     Just ((l_val, r_val), input'')
{-
  case parsePrefix schemal input of
       Nothing => Nothing
       Just (l_val, input') =>
            case parsePrefix schemar input' of
                 Nothing => Nothing
                 Just (r_val, input'') => Just ((l_val, r_val), input'')
-}

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Nothing => Nothing
                                  (Just (x, "")) => Just x
                                  (Just _) => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs)
  = case xs of
         [] => Just SString
         _ => case parseSchema xs of
                   Nothing => Nothing
                   Just xs_sch => Just (SString .+. xs_sch)

parseSchema ("Char" :: xs)
  = case xs of
         [] => Just CChar
         _ => case parseSchema xs of
                   Nothing => Nothing
                   Just xs_sch => Just (CChar .+. xs_sch)
                   
parseSchema ("Int" :: xs)
  = case xs of
         [] => Just SInt
         _  => case parseSchema xs of
                    Nothing => Nothing
                    Just xs_sch => Just (SInt .+. xs_sch)
parseSchema _ = Nothing


parseCommand : (schema: Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" y = case parseBySchema schema y of
                                   Nothing => Nothing
                                   Just yok => Just (Add yok)
                                   
-- parseCommand schema "search" y = Just (Search y)

parseCommand schema "get" "" = Just (Get (-1))

parseCommand schema "get" y = case all isDigit (unpack y) of
                            False => Nothing
                            True => Just (Get (cast y))
parseCommand schema "quit" "" = Just Quit
parseCommand schema "schema" rest =
  do schemaok <- parseSchema (words rest)
     Just (SetSchema schemaok)
        
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)


setSchema : (Store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              S k => Nothing
                              
processInput : DataStore -> String -> Maybe (String, DataStore)
 
processInput store inp = case parse (schema store) inp of
                              Nothing => Just ("Invalid command\n", store)
                              (Just (Add x)) => 
                                Just ("ID " ++ show (size store) ++ "\n", addToStore store x)
                              Just (SetSchema schema') =>
                                case setSchema store schema' of
                                     Nothing => Just ("Can't update schema\n", store)
                                     Just store' => Just ("OK\n", store')
                              (Just (Get (-1))) => Just ((getAllEntries store), store)
                              (Just (Get x)) => getEntry x store
                              (Just Quit) => Nothing


main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ [])
                "Command: " processInput

