signature TABLE = 
sig
   type key
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val look  : 'a table * key -> 'a option
   val items : 'a table -> 'a list
   val keys  : 'a table -> (IntBinaryMap.Key.ord_key * 'a) list
end

