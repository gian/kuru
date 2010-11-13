structure Debug =
struct
   (* 0 = silent *)
   (* 1 = No warnings *)
   (* 2 = Errors and warnings *)
   (* 7 = Noisy! *)

   val debugLevel = ref 3

   fun setDebugLevel i = debugLevel := i
   fun getDebugLevel () = !debugLevel

	val silent = 0
	val error = 1
	val warning = 2
	val info = 3
	val verbose = 4
	val internal = 5
	val debug = 6
	val debugmore = 7

   fun lName 1 = ""
     | lName 2 = "warning: "
     | lName 3 = ""
     | lName 4 = ""
     | lName 5 = "[internal] "
     | lName 6 = "[debug] "
     | lName 7 = "[debugmore] "
     | lName _ = "?"

   fun print l m = if l <= !debugLevel then 
         TextIO.print (lName l ^ m) else () 

end

