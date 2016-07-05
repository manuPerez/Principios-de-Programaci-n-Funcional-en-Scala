/*
Anonymous Functions:

Passing functions as parameters leads to the creation of many small
functions.

Is like 'functions literals', which let us write a function without givin
it a name.


Anonymous Functions are Sintactic Sugar.

An anonymous function can always be expressed using def as follows:

    anonymous function: (x1: t1, ..., xN: tN) => E
    def function:       def f(x1: t1, ..., xN: tN) => E

where 'f' is an arbitrary, fresh name (that's not yet used in the program)

One can therefore say that anonymous functions are syntactic sugar.

*/

def sum(f: Int => Int, a: Int, b: Int): Int = {
  if(a > b) 0 else f(a) + sum(f, a + 1, b)
}

def sumInts(a: Int, b: Int): Int = sum(x => x, a, b)
def sumCubes(a: Int, b: Int): Int = sum(x => x * x * x, a, b)

// These functions aren't neccessary anymore
// def id(x: Int): Int = x
// def cube(a: Int): Int = a * a * a

sumInts(1, 5)
sumCubes(1, 5)
