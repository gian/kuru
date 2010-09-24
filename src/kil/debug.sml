structure Debug =
struct
   (* 0 = silent *)
   (* 1 = No warnings *)
   (* 2 = Errors and warnings *)
   (* 7 = Noisy! *)

   val debugLevel = ref 1

   fun setDebugLevel i = debugLevel := i
   fun getDebugLevel () = !debugLevel

   fun lName 1 = "error"
     | lName 2 = "warning"
     | lName 3 = "info"
     | lName 4 = "verbose"
     | lName 6 = "interp"
     | lName 7 = "debug7"
     | lName _ = "?"

   fun print l m = if l <= !debugLevel then 
         TextIO.print ("[" ^ lName l ^ "] " ^ m) else ()



end

