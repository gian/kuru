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
*    Generate documentation from annotated parse trees. 
******************************************************************************)

structure DocGen =
struct

   open ParseTree

   fun dg (Ident (s,_)) = "<identifier>" ^ s ^ "</identifier>"
     | dg (Var (p,_)) = "'" ^ dg p
     | dg (TyId (p,_)) = dg p
     | dg (LongId (l1,l2,_)) = "<longid>\n" ^ 
                               "  <prefix>" ^ dg l1 ^ "</prefix>\n" ^ 
                               "  <identifier>" ^ dg l2 ^ "</identifier>\n" ^
                               "</longid>"
     | dg (Seq (p1, p2,_)) = dg p1 ^ "\n" ^ dg p2
     | dg (TyAnn (p1,p2,_)) = "<typedexpr>\n" ^ 
                              "  <expr>" ^ dg p1 ^ "</expr>\n" ^ 
                              "  <type>" ^ dg p2 ^ "</type>\n" ^
                              "</typedexpr>"
     | dg (TyAnnPat (i,p,_)) = 
         "<pattern>\n" ^ 
         "  <type>" ^ dg p ^ "</type>\n" ^ 
         "  <body>" ^ dg i ^ "</body>\n" ^
         "</pattern>\n"
     | dg (IdPat (i,p)) = dg i
     | dg (TyArrow (t1,t2,_)) = dg t1 ^ " -> " ^ dg t2
     | dg (TyPair (t1,_)) = String.concatWith " * " (map dg t1) 
     | dg (TyCon (t1,t2,_)) = dg t1 ^ " " ^ dg t2
     | dg (TyName (t1,_)) = dg t1
     | dg (ValDec (b,_)) =
          String.concatWith "\n  " (map dg b) 
     | dg (TypeDec (b,_)) =
         String.concatWith "\n  " (map dg b)
     | dg (InfixrDec (p,opr,_)) = 
         "<dec>\n" ^ 
         "<infixr prec=\"" ^ p ^ "\">" ^ dg opr ^ "</infixr>\n" ^
         "</dec>\n"  
     | dg (InfixDec (p,opr,_)) = 
         "<dec>\n" ^ 
         "<infix prec=\"" ^ p ^ "\">" ^ dg opr ^ "</infixr>\n" ^
         "</dec>\n"  
     | dg (DatatypeAssign (i1,i2,_)) = 
         "<dec>\n" ^ 
          "<datatype>\n" ^ 
          "  <name>" ^ dg i1 ^ "</name>\n" ^ 
          "  <body>" ^ dg i2 ^ "</body>\n" ^
          "</datatype>\n" ^
         "</dec>\n" 
     | dg (DatatypeDec (d,_)) =
          String.concatWith "\n" (map dg d)
     | dg (FunDec (d,_)) = 
          String.concatWith "\n" 
            (map (fn x => String.concatWith "\n" (map dg x)) d)
     | dg (ValBind (p,e,_)) = 
         "<dec>\n" ^ 
          "<valbind>\n" ^ 
          "  <name>" ^ dg p ^ "</name>\n" ^ 
          "  <body>" ^ dg e ^ "</body>\n" ^
          "</valbind>\n" ^ 
         "</dec>\n" 
	  | dg (ValRec (b,_)) =
         "<dec>\n" ^ 
          "<valbind recursive=\"true\">\n" ^ 
            String.concatWith "\n" (map dg b) ^ 
          "</valbind>\n" ^ 
         "</dec>\n" 
     | dg (TypeBind (v,p,e,_)) = 
         "<dec>\n" ^ 
          "<typebind>\n" ^
          "  <tyvars>" ^ 
          (String.concatWith "," (map dg v)) ^
          "</tyvars>\n" ^
          "  <name>" ^ dg p ^ "</name>\n" ^
          "  <body>" ^ dg e ^ "</body>\n" ^
          "</typebind>\n" ^
         "</dec>\n" 
     | dg (FunMatch (i,l,NONE,e,_)) = 
         "<dec>\n" ^ 
          "<fun>\n" ^ 
          "  <name>" ^ dg i ^ "</name>\n" ^
          "  <params>" ^        
             (String.concatWith " " (map dg l)) ^ 
          "</params>\n" ^
          "</fun>" ^ 
         "</dec>\n" 
     | dg (FunMatch (i,l,SOME t,e,_)) = 
         "<dec>\n" ^ 
          "<fun>\n" ^ 
          "  <name>" ^ dg i ^ "</name>\n" ^
          "  <type>" ^ dg t ^ "</type>\n" ^
          "  <params>" ^        
             (String.concatWith " " (map dg l)) ^ 
          "</params>\n" ^
          "</fun>" ^
         "</dec>\n" 
     | dg (DataBind (tv,i,b,_)) = 
         "<dec>\n" ^ 
          "<datatype>\n" ^
          "  <tyvars>" ^ 
          (String.concatWith "," (map dg tv)) ^
          "</tyvars>\n" ^
          "  <name>" ^ dg i ^ "</name>\n   " ^
          "  <body>\n" ^ 
          (String.concatWith "\n" (map dg b)) ^ 
          "  </body>\n" ^
          "</datatype>\n" ^
         "</dec>\n" 
     | dg (ConBind (i,NONE,_)) = 
          "    <con>\n" ^
          "      <name>" ^ dg i ^ "</name>\n" ^
          "    </con>"
     | dg (ConBind (i,SOME t,_)) =
          "    <con>\n" ^
          "      <name>" ^ dg i ^ "</name>\n" ^
          "      <type>" ^ dg t ^ "</type>\n" ^
          "    </con>"
     | dg (StructDec (l,_)) =
          String.concatWith "\n" (map dg l)
     | dg (StructBind (i,b,_)) = 
         "<dec>\n" ^ 
          "<structure>\n" ^ 
          "  <name>" ^ dg i ^ "</name>\n" ^
          "  <body>" ^ dg b ^ "</body>\n" ^ 
          "</structure>\n" ^ 
         "</dec>\n" 
     | dg (Structure (l,_)) =
          "<struct>\n" ^ 
          (String.concatWith "\n   " (map dg l)) ^ 
          "</struct>"
     | dg (DocNode ("",p)) = 
          dg p ^ "\n" 
     | dg (DocNode (s,p)) = 
          "<doc>\n" ^ 
          "  <description>" ^ s ^ "</description>\n" ^ 
          "  <body>" ^ dg p ^ "</body>\n" ^
          "</doc>\n"
     | dg (FileNode (f,p)) = 
          "<file>\n" ^ 
          "<name>" ^ f ^ "</name>\n" ^ 
          (String.concatWith "\n" (map dg p)) ^
          "</file>\n"
     | dg x = ""

   fun docGen p = dg p
  
   fun genDocs output l =
      let
         val fp = TextIO.openOut output
         val p =
      "<?xml version=\"1.0\"?>\n" ^
      "<?xml-stylesheet type=\"text/xsl\" href=\"kdoc.xsl\"?>\n" ^
      "<kdoc>\n" ^ 
      (String.concatWith "\n" (map docGen l))  ^
      "\n</kdoc>\n"
         val _ = TextIO.output(fp,p)
         val _ = TextIO.closeOut fp
      in
         () 
      end
end


