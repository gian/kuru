datatype inttree = Leaf of int
                 | Branch of inttree 


val p = Leaf 10

val q = Branch (Branch p)