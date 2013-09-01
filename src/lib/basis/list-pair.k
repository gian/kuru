structure ListPair =
struct
   fun zip (h::t, h'::t') = (h,h') :: zip (t, t')
   fun unzip ((a,b)::t) = ([a],[b])

end

