structure OS =
struct

	structure Process =
	struct
      fun exit x = ()
		val failure = 1 
      fun getEnv s = ""
      fun isSuccess x = false
      fun system x = ()
	end

	structure FileSys =
	struct
		datatype ac = A_READ | A_WRITE | A_EXEC

		fun access x = false

      fun tmpName () = "tmp"

      fun mkDir "foo" = ()
	end

	structure Path =
	struct

	end

end
