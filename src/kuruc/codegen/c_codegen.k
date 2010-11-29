(*******************************************************************************
*  The Kuru Programming Language Compiler Toolset (http://www.kuru-lang.org)
*  Copyright (C) 2010  Gian Perrone
*
*  This program is free software: you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation, either version 3 of the License, or
*  (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*******************************************************************************
*  This file:
*    C backend code generator.
******************************************************************************)
structure CCodeGen =
struct
   val j = String.concat
   val jsp = String.concatWith " "
   val jnl = String.concatWith "\n"
   val jc = String.concatWith ", "

   fun ind' 0 = ""
     | ind' n = "   " ^ ind' (n-1)

   fun ind n s = 
      ind' n ^ s

   fun codegen (pre, stms) =
      jnl [
         "#include <stdlib.h>",
         "#include <stdio.h>",
         "#include <string.h>\n",
         "void RAISE (const char *s) {",
         "   fprintf(stderr,\"Exception: %s\\n\", s);",
         "   exit(1);",
         "}\n",
         jnl (map jnl pre),
         "int main (int argc, char** argv) {\n",
         "      io_init ();",
         "      int_init ();",
         "      string_init ();",
         jnl (map (ind 2) stms),
         "      io_done ();",
         "      int_done ();",
         "      string_done ();",
         "      return 0;\n}\n"
      ]

   fun emit code =
      let
         val dirName = (OS.FileSys.tmpName ()) ^ ".kuruc"
         val _ = Debug.print Debug.verbose ("Creating output directory: " ^ 
                                             dirName ^ "\n")
         val _ = OS.FileSys.mkDir dirName
         val fp = TextIO.openOut (dirName ^ "/main.c")

         fun includeLib s = 
            let
               val g = Config.resolveLibrary s 
               val lib = TextIO.openIn g
               val lib' = TextIO.input lib
               val _ = TextIO.closeIn lib
               val _ = TextIO.output (fp, lib' ^ "\n")
            in
               ()
            end
         
         val _ = includeLib "io_kuru.h"
         val _ = includeLib "int_kuru.h"
         val _ = includeLib "string_kuru.h"
         
         (* Final code emit *)
         val _ = TextIO.output (fp, code)
         val _ = TextIO.closeOut (fp)

         val _ = Debug.print Debug.verbose ("Output C file: " ^ 
                                             dirName ^ "/main.c" ^ "\n")

         val fp = TextIO.openOut (dirName ^ "/kuru_types.h")

         val _ = TextIO.output (fp,
            "#ifndef _KURU_TYPES \n\
           \ #define _KURU_TYPES 1 \n\
           \ typedef struct { \n\
           \    int length; \n\
           \    char *data; \n\
           \ } kuru_string_t; \n\n\
           \ typedef struct kuru_list_t { \n\
           \    char *data; \n\
           \    struct kuru_list_t *next; \n\
           \ } kuru_list_t; \n\
           \ typedef struct closure_t { \n\
           \    void *(*func)(struct closure_t*, void *data); \n\
           \ } closure_t; \n\n\
           \ typedef struct { \n\
           \    int i; \n\
           \    void *data; \n\
           \ } constr_t; \n\n\
           \ typedef void* kuru_unit_t; \n\n\
           \ typedef int kuru_bool_t; \n\n\
           \ #define UNIT ((void *) 0) \n\
           \ #define TRUE 1 \n\
           \ #define FALSE 0 \n\
           \ #define true 1 \n\
           \ #define false 0 \n\
           \ #define KMALLOC(x) malloc(x) \n\
           \ #define KSTRDUP(x) c2ks(x) \n\
           \ kuru_string_t *c2ks(const char *cstr); \n\
           \ void kuru_string_destroy(kuru_string_t *kstr); \n\
           \ kuru_string_t *ksdup(const kuru_string_t *kstr); \n\
           \ #endif\n")

         val _ = TextIO.closeOut (fp)

         val _ = Debug.print Debug.verbose ("Output types file: " ^ 
                                             dirName ^ "/kuru_types.h" ^ "\n")

      in
         dirName 
      end handle OS.SysErr (m,NONE) => raise Fail ("Cannot emit code: " ^ m)
               | OS.SysErr (m,SOME se) => raise Fail ("Cannot emit code: " ^ 
                                                      m ^ ": " ^ OS.errorMsg se)
end
