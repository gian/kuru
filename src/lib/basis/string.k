structure String =
struct
   fun concat l = ""
   fun concatWith d l = ""
   fun str c = ""
   fun tokens f s = []
   fun fields f s = []
   fun explode s = []
   fun implode [] = ""
   fun size "" = 0
   fun substring ("",0,0) = ""
   fun isSuffix "" "" = false
end

val size = String.size

