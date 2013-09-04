
structure IUnionFind :> IMPERATIVE_UNION_FIND
   =
   struct

      datatype 'a state =
         Root of 'a * int
       | Child of 'a state ref 

      type 'a set = 'a state ref

      fun new label = ref (Root (label, 0))

      val eq = (fn (x, y) => x = y)

      fun root (set : 'a set) =
         (case !set of
             Root _ =>
                set

           | Child parent =>
                let
                   val ancestor = root parent
                in
                   set := Child ancestor;
                   ancestor
                end)

      fun union f set1 set2 =
         let
            (* root always returns a Root state *)
            val ancestor1 = root set1
            val (ref (Root (label1, rank1))) = ancestor1 
            val ancestor2 = root set2
            val (ref (Root (label2, rank2))) = ancestor2 
         in
            if ancestor1 = ancestor2 then
               ()
            else if rank1 < rank2 then
               (
               ancestor1 := Child ancestor2;
               ancestor2 := Root (f (label2, label1), rank2)
               )
            else if rank1 > rank2 then
               (
               ancestor2 := Child ancestor1;
               ancestor1 := Root (f (label1, label2), rank1)
               )
            else (* rank1 = rank2 *)
               (
               ancestor2 := Child ancestor1;
               ancestor1 := Root (f (label1, label2), rank1+1)
               )
         end

      fun sameSet (set1, set2) = root set1 = root set2

      fun find set =
         (case ! (root set) of
             Root (label, _) => label
           | _ => raise (Fail "invariant"))

      fun isCanonical set =
         (case ! set of
             Root _ => true
           | Child _ => false)

      fun indent 0 = ""
        | indent i = "   " ^ indent (i-1)

      fun toString' ind ts (Root (a, i)) = 
            indent ind ^ ts a ^ ": " ^ Int.toString i ^ "\n"
        | toString' ind ts (Child s) = toString' (ind+1) ts (!s)

      fun toString ts s = toString' 0 ts (!s)
   end
