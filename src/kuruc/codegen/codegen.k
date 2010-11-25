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
*    Code generate LLVM output from IR. 
******************************************************************************)
structure CodeGen =
struct
   structure T = Translate

   val j = String.concat
   val jsp = String.concatWith " "
   val jnl = String.concatWith "\n"
   val jc = String.concatWith ", "
   val dquote = "\""

   fun ind 0 = ""
     | ind n = "   " ^ ind (n-1)

   fun cg_ty Temp.i1 = "i1"
     | cg_ty Temp.i8 = "i8"
     | cg_ty Temp.i16 = "i16"
     | cg_ty Temp.i32 = "i32"
     | cg_ty Temp.i64 = "i64"
     | cg_ty Temp.r32 = "r32"
     | cg_ty Temp.r64 = "r64"
     | cg_ty (Temp.label _) = "" 
     | cg_ty (Temp.global (_,t)) = cg_ty t 
     | cg_ty (Temp.ptr t) = cg_ty t ^ "*"
     | cg_ty Temp.void = "void"
     | cg_ty (Temp.fn_ptr (t1,t2)) = j [cg_ty t2,
                                       " (",
                                       jc (map cg_ty t1),
                                       ") *"]
     | cg_ty (Temp.aggr l) = j ["{ ",
                               jc (map cg_ty l),
                               " }"]
     | cg_ty (Temp.array (i,t)) = j [ "[", Int.toString i, " x ", cg_ty t, "]" ]
     | cg_ty (Temp.self_ref i) = j ["\\",Int.toString i]
     | cg_ty (Temp.named s) = "%" ^ s

   val ppt = Temp.prettyPrint
   val ppt_np = Temp.prettyPrintNoPrefix 

   fun opt_t (true,s) = s ^ " "
     | opt_t (false,s) = ""

 (*  fun cg_frag' (T.FSTRING (l,s)) = 
      j [ppt l, " = ", 
         "private constant ",
           "[", Int.toString (size s + 1), " x i8] c",
          dquote, s, "\\00", dquote]
     | cg_frag' (T.FFN (l,exps,stms,t)) =
         j [
            jsp ["define ",cg_ty t,ppt l,"("],
            jc (map cg_exp_t exps), ") {\n",
            jnl (map (cg_stm 2) stms),
            "\n}\n"
         ]
     | cg_frag' (T.FALIAS (e1,e2)) =
         j [cg_exp e1, " = alias ", cg_exp_t e2]
     | cg_frag' (T.FPOLYFN (l,_,_,Temp.fn_ptr(a,b))) =
          j [
            jsp ["define ",cg_ty b,ppt l,"("],
            jsp [cg_ty (hd a), "%x"], 
            ") {\n",
            "      unwind",
            "\n}\n"
         ]
     | cg_frag' _ = Error.bug ("cg_frag","Invalid POLYFN fragment",(0,""))
        
   and cg_frag l = (cg_frag' l)*)

   fun cg_frag _ = "; FRAG WOULD GO HERE MAYBE" (* FIXME *)

   and cg_cmp (Asm.REQ) _ _ = raise Fail "cg_cmp: REQ not implemented"
     | cg_cmp (Asm.RNEQ) _ _ = raise Fail "cg_cmp: RNEQ not implemented"
     | cg_cmp opr e1 e2 =
       let
          val opr' = 
           (case opr of 
               Asm.EQ => "eq"
             | Asm.NEQ => "ne"
             | Asm.LT => "slt"
             | Asm.GT => "sgt"
             | Asm.LE => "sle"
             | Asm.GE => "sge"
             | Asm.ULT => "ult"
             | Asm.ULE => "ule"
             | Asm.UGT => "ugt"
             | Asm.UGE => "uge"
             | _ => "???")
       in
          j ["icmp ", opr', " ",
             cg_exp_t e1, ", ", cg_exp e2]
       end

   and cg_exp' b (Asm.CONST i) = j [opt_t (b,
                                         cg_ty (Temp.getMachineWord ())),
                                  Int.toString i]
     | cg_exp' b (Asm.RCONST i) = j [opt_t (b,"r32"), Real.toString i]
     | cg_exp' b (Asm.BCONST i) = j [opt_t (b, "i1"), if i then "true" else "false"]
     | cg_exp' b (Asm.EXN _) = j [opt_t (b, "i32"), "123456"] (* FIXME *)
     | cg_exp' b (Asm.NAME l) = j [opt_t (b, cg_ty (Temp.getType l)), ppt l]
     | cg_exp' b (Asm.BUILTIN s) = "@" ^ s
     | cg_exp' b (Asm.NULL t) = j [opt_t (b, cg_ty t), "null"]
     | cg_exp' b (Asm.TEMP t) = j [opt_t (b,cg_ty (Temp.getType t)), ppt t]
     | cg_exp' b (Asm.CALL (ty,e1,e2)) = 
         jsp ["call ",cg_ty ty,cg_exp e1,
                j ["(", cg_exp_t e2, ")"]
             ]
     | cg_exp' b (Asm.TUPLE_IND (e,i)) =
         j [
            "extractvalue ", cg_exp_t e, ", ", Int.toString i 
         ]
     | cg_exp' b (Asm.TUPLE_INS (e1,e2,i)) =
         j [
            "insertvalue ", cg_exp_t e1, ", ", cg_exp_t e2, ", ", Int.toString i
         ]
     | cg_exp' b (Asm.ELEM_PTR (ty,e1,i)) =
         j [
            "getelementptr ", cg_ty ty, "* ", cg_exp e1, ", i32 0, i32 ",
            (String.concatWith ", i32 " (map Int.toString i))
         ]
     | cg_exp' b (Asm.VOID) = 
         j [opt_t (b,"void"), "void"]
     | cg_exp' b (Asm.UNDEF t) =
         j [opt_t (b,cg_ty t), "undef"]
     | cg_exp' _ (Asm.BINOP (opr,a,b)) =
        let
           val opr' = 
           (case opr of
               "+" => "add"
             | "-" => "sub"
             | "*" => "mul"
             | "/" => "sdiv"
             | "=" => "icmp eq"
             | "<>" => "icmp ne"
             | "<" => "icmp slt"
             | ">" => "icmp sgt"
             | "<=" => "icmp sle"
             | ">=" => "icmp sge"
             | "andalso" => "and"
             | "orelse" => "or"
             | _ => raise Fail ("Invalid BINOP "^ opr ^" in cg_exp"))
        in
           j [opr', " ", cg_exp_t a, ", ", cg_exp b]
        end
     | cg_exp' b (Asm.ALLOCA t) = 
            j ["alloca ", cg_ty t]
     | cg_exp' b (Asm.MALLOC (t,n)) =
            j ["tail call i8* @malloc(i32 ptrtoint (",
               cg_ty (Temp.ptr t),
               " getelementptr (",
               cg_ty (Temp.ptr t),
               " null, i32 ",
               Int.toString n,
               ") to i32))"]
     | cg_exp' b (Asm.LOAD e) =
             j ["load ", cg_exp_t e]
     | cg_exp' b (Asm.BITCAST (t,ty)) =
             j ["bitcast ",
                cg_exp_t t,
                " to ", cg_ty ty]
     | cg_exp' b (Asm.ALIAS e) = 
             j ["alias ", cg_exp e]
     | cg_exp' b (Asm.CMP (r,e1,e2)) =
             cg_cmp r e1 e2
     | cg_exp' b _ = raise Fail "cg_exp' b: Not implemented"

   and cg_exp_t e = (cg_exp' true e)
   
   and cg_exp e = (cg_exp' false e)

   and cg_stm i (Asm.MOVE (e1,e2)) = 
          j [ind i, cg_exp e1, " = ", cg_exp e2]
     | cg_stm i (Asm.EXP e1) =
          j [ind i, cg_exp e1]
     | cg_stm i (Asm.JUMP (e,l)) =
          j [ind i, "br label ", cg_exp e]
     | cg_stm i (Asm.CJUMP (e,l1,l2)) =
       j [ind i, "br ", cg_exp_t e, ", label ", ppt l1, ", label ", ppt l2]
     | cg_stm i (Asm.PHI (e, t, el)) =
       let
          val el' = 
            map (fn (ex,la) => j [ "[", cg_exp ex, ", ", ppt la, "]" ]) el
       in
          j [ ind i,
             cg_exp e, " = phi ", cg_ty t, " ", jc el' 
          ]
       end
     | cg_stm i (Asm.LABEL l) =
          j [ind (i-1), ppt_np l, ":"]
     | cg_stm i (Asm.RET e) =
          j [ind i, "ret ", cg_exp_t e]
     | cg_stm i (Asm.UNWIND) =
          j [ind i, "unwind"]
     | cg_stm i (Asm.STORE (e1,e2)) =
          j [ind i, "store ", cg_exp_t e1, ", ", cg_exp_t e2]
     | cg_stm i (Asm.COMMENT s) = 
          j [ind i, "; ", s]
     | cg_stm i (Asm.UNREACHABLE) =
          j [ind i, "unreachable"]
     | cg_stm i (Asm.DECLARE (ot,n,it)) =
          j [ind i, "declare ", cg_ty ot, " @", n, " (", cg_ty it, ")"]

   fun codegen (stms, frags) =
      jnl [
         jnl (map cg_frag frags),
         "define i32 @main (i32 %argc, i8** %argv) {\n   entry:\n",
         "      call void @io_init ()",
         "      call void @int_init ()",
         jnl (map (cg_stm 2) stms),
         "      br label %finalise",
         "   finalise:",
         "      call void @int_done ()",
         "      ret i32 0\n}\n"
      ]

   fun emit code =
      let
         val dirName = (OS.FileSys.tmpName ()) ^ ".kuruc"
         val _ = Debug.print Debug.verbose ("Creating output directory: " ^ 
                                             dirName ^ "\n")
         val _ = OS.FileSys.mkDir dirName
         val fp = TextIO.openOut (dirName ^ "/main.ll")

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
         
         val _ = includeLib "gc.ll"
         val _ = includeLib "io_kuru.ll"
         val _ = includeLib "int_kuru.ll"
         
         (* Final code emit *)
         val _ = TextIO.output (fp, code)
         val _ = TextIO.closeOut (fp)

         val _ = Debug.print Debug.verbose ("Output LLVM asm file: " ^ 
                                             dirName ^ "/main.ll" ^ "\n")
      in
         dirName 
      end handle OS.SysErr (m,NONE) => raise Fail ("Cannot emit code: " ^ m)
               | OS.SysErr (m,SOME se) => raise Fail ("Cannot emit code: " ^ 
                                                      m ^ ": " ^ OS.errorMsg se)
end
