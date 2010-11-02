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
   structure I = Intermediate
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
     | cg_ty (Temp.label _) = raise Fail "CodeGen: taking type of label"
     | cg_ty (Temp.global _) = raise Fail "CodeGen: taking type of global"
     | cg_ty (Temp.ptr t) = cg_ty t ^ "*"
     | cg_ty Temp.void = "void"
     | cg_ty (Temp.fn_ptr (t1,t2)) = j [cg_ty t2,
                                       " (",
                                       cg_ty t1,
                                       ") *"]
     | cg_ty (Temp.aggr l) = j ["{ ",
                               jc (map cg_ty l),
                               " }"]
     | cg_ty (Temp.array (i,t)) = j [ "[", Int.toString i, " x ", cg_ty t, "]" ]

   val ppt = Temp.prettyPrint
   val ppt_np = Temp.prettyPrintNoPrefix 

   fun opt_t (true,s) = s ^ " "
     | opt_t (false,s) = ""

   fun cg_frag' (T.FSTRING (l,s)) = 
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
   and cg_frag l = (cg_frag' l)

   and cg_cmp t (I.REQ) _ _ = raise Fail "cg_cmp: REQ not implemented"
     | cg_cmp t (I.RNEQ) _ _ = raise Fail "cg_cmp: RNEQ not implemented"
     | cg_cmp t opr e1 e2 =
       let
          val opr' = 
           (case opr of 
               I.EQ => "eq"
             | I.NEQ => "ne"
             | I.LT => "slt"
             | I.GT => "sgt"
             | I.LE => "sle"
             | I.GE => "sge"
             | I.ULT => "ult"
             | I.ULE => "ule"
             | I.UGT => "ugt"
             | I.UGE => "uge"
             | _ => "???")
       in
          j [ppt t, " = icmp ", opr', " ",
             cg_exp_t e1, ", ", cg_exp e2]
       end

   and cg_exp' b (I.CONST i) = j [opt_t (b,
                                         cg_ty (Temp.getMachineWord ())),
                                  Int.toString i]
     | cg_exp' b (I.RCONST i) = j [opt_t (b,"r32"), Real.toString i]
     | cg_exp' b (I.BCONST i) = j [opt_t (b, "i1"), if i then "true" else "false"]
     | cg_exp' b (I.EXN _) = j [opt_t (b, "i32"), "123456"] (* FIXME *)
     | cg_exp' b (I.NAME l) = ppt l
     | cg_exp' b (I.BUILTIN s) = "@" ^ s
     | cg_exp' b (I.TEMP t) = j [opt_t (b,cg_ty (Temp.getType t)), ppt t]
     | cg_exp' b (I.CALL (ty,e1,e2)) = 
         jsp ["call ",cg_ty ty,cg_exp e1,
                j ["(", cg_exp_t e2, ")"]
             ]
     | cg_exp' b (I.TUPLE_IND (e,i)) =
         j [
            "extractvalue ", cg_exp_t e, ", ", Int.toString i 
         ]
     | cg_exp' b (I.TUPLE_INS (e1,e2,i)) =
         j [
            "insertvalue ", cg_exp_t e1, ", ", cg_exp_t e2, ", ", Int.toString i
         ]
     | cg_exp' b (I.ELEM_PTR (ty,e1,i)) =
         j [
            "getelementptr ", cg_ty ty, "* ", cg_exp e1, ", i32 0, i32 ",
            Int.toString i
         ]
     | cg_exp' b (I.VOID) = 
         j [opt_t (b,"void"), "void"]
     | cg_exp' b (I.UNDEF t) =
         j [opt_t (b,cg_ty t), "undef"]
     | cg_exp' _ (I.BINOP (opr,a,b)) =
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
             | "_andalso" => "and"
             | "_orelse" => "or"
             | _ => raise Fail "Invalid BINOP in cg_exp")
        in
           j [opr', " ", cg_exp_t a, ", ", cg_exp b]
        end
     | cg_exp' b (I.ALLOCA t) = 
            j ["alloca ", cg_ty t]
     | cg_exp' b _ = raise Fail "cg_exp' b: Not implemented"

   and cg_exp_t e = (cg_exp' true e)
   
   and cg_exp e = (cg_exp' false e)

   and cg_stm i (I.MOVE (e1,e2)) = 
          j [ind i, cg_exp e1, " = ", cg_exp e2]
     | cg_stm i (I.EXP e1) =
          j [ind i, cg_exp e1]
     | cg_stm i (I.JUMP (e,l)) =
          j [ind i, "br label ", cg_exp e]
     | cg_stm i (I.CJUMP (e,l1,l2)) =
       j [ind i, "br ", cg_exp_t e, ", label ", ppt l1, ", label ", ppt l2]
     | cg_stm i (I.CJUMP2 (r,e1,e2,l1,l2)) =
       let
          val c = Temp.newtemp (Temp.i1)
       in
          jnl [
             j [ind i, cg_cmp c r e1 e2],
             j [ind i, "br i1 ", ppt c, ", ",
                jc [
                   j ["label ", ppt l1],
                   j ["label ", ppt l2]
                ]
             ]
          ]
       end
     | cg_stm i (I.PHI (e, t, el)) =
       let
          val el' = 
            map (fn (ex,la) => j [ "[", cg_exp ex, ", ", ppt la, "]" ]) el
       in
          j [ ind i,
             cg_exp e, " = phi ", cg_ty t, " ", jc el' 
          ]
       end
     | cg_stm i (I.LABEL l) =
          j [ind (i-1), ppt_np l, ":"]
     | cg_stm i (I.RET e) =
          j [ind i, "ret ", cg_exp_t e]
     | cg_stm i (I.UNWIND) =
          j [ind i, "unwind"]
     | cg_stm i (I.STORE (e1,e2)) =
          j [ind i, "store ", cg_exp_t e1, ", ", cg_exp_t e2]
     | cg_stm i _ = raise Fail "cg_stm: Not implemented"

   fun codegen (stms, frags) =
      jnl [
         jnl (map cg_frag frags),
         "define i32 @main (i32 %argc, i8** %argv) {\n   entry:\n",
         "      call void @io_init ()",
         jnl (map (cg_stm 2) stms),
         "      br label %finalise",
         "   finalise:",
         "      call void @io_done ()",
         "      ret i32 0\n}\n"
      ]

   fun emit code =
      let
         val dirName = (OS.FileSys.tmpName ()) ^ ".kuruc"
         val _ = Debug.print Debug.verbose ("Creating output directory: " ^ 
                                             dirName ^ "\n")
         val _ = OS.FileSys.mkDir dirName
         val fp = TextIO.openOut (dirName ^ "/main.ll")

         (* Include the IO library -
              This is a bit dirty and hardcoded at the moment,
              so it qualifies for a FIXME *)
         val io = Config.resolveLibrary "io_kuru.ll"
         val iolib = TextIO.openIn io
         val iolib' = TextIO.input iolib
         val _ = TextIO.closeIn iolib

         val _ = TextIO.output (fp, iolib' ^ "\n")
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
