
datatype 'a tree = Node of 'a tree * 'a * 'a tree 
                 | Leaf of 'a

val t = Node (Leaf 1, 2, Leaf 3)

fun f (Leaf _) = 1


