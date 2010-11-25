%string = type { i32, i8* }
%unit = type i8*
%closure_cc1_t = type { %unit (%closure_cc1_t *, %string)* , void (%string)* }
declare i8* @malloc(i32)