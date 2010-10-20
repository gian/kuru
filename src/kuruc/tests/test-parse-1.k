
datatype 'a option = NONE | SOME of 'a

datatype foo = Real of int * int * (int * int) option

fun pp (Real (b,s1,s2,SOME (b2,s3))) = "hello"