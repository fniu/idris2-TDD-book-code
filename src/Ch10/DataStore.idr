module Ch10.DataStore

import Data.Vect

infixr 5 .+.


public export
data Schema = SString | SInt | (.+.) Schema Schema

public export
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)


export
record DataStore (schema : Schema) where
  constructor MkData
  size : Nat
  items : Vect size (SchemaType schema)
  
export
empty : DataStore schema
empty = MkData 0 []

export
addToStore : (value : SchemaType schema) -> (store : DataStore schema) ->
             DataStore schema
addToStore value (MkData _ items) = MkData _ (value :: items)

       
public export
data StoreView : (schema : _) -> DataStore schema -> Type where
  SNil : StoreView schema Ch10.DataStore.empty
  SAdd : (value, store : _) -> (rec : StoreView schema store) ->
         StoreView schema (addToStore value store)
 
storeViewHelp : {size : _} ->
                (items : Vect size (SchemaType schema)) ->
                StoreView schema (MkData size items)
                 
