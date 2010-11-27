%string = type { i32, i8* }*
%unit = type i8*
%closure_cc1_t = type { %unit (%closure_cc1_t *, %string)* }
%closure_cc2_t = type { %unit (%closure_cc1_t *, %string)* }
%closure_t = type { i8* }
declare i8* @malloc(i32)