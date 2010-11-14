val a = (10 : int) * 5

type program = string

type foo = bool * string * string * (bool * string) option

type k = int * int * program list 


fun x z = z
val bar = x 10 : int

type b = int
type c = int
type d = int
type e = int
type f = int

type a = b * c * d * e

type f = (b * c) * (d * (e * f))