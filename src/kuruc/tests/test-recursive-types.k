datatype 'a foo = Foo of 'a * 'a foo
                | Bar of 'a bar

and 'a bar = Blah of int
           | Bloop of 'a * 'a baz

withtype 'a baz = 'a bar foo

val p = Foo (234, Bar (Bloop (3, Blah 1)))


