datatype 'a foo = Bla of 'a
datatype 'a bar = Bar of 'a foo

type 'a bla = 'a bar foo

val p : int bla = Bla (Bar (Bla 1))




