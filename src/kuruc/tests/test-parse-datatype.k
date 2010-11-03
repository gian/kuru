datatype inttree = Leaf of int
                 | Branch of inttree 


val p = Leaf 10

val q = Branch (Branch p)

fun extract (Leaf i) = i
  | extract (Branch b) = extract b
