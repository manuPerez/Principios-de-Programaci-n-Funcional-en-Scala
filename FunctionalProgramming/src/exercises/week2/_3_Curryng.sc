/*
Note that 'a' and 'b' get passed unchanged from sumInts and sumCubes
into sum.

Can we be even shorter by getting rid of these parameters?

def sum(f: Int => Int, a: Int, b: Int): Int = {
    if(a > b) 0
    else f(a) + sum(f,
        a + 1, b)
}

def sumInts(a: Int, b: Int): Int = sum(x => x, a, b)
def sumCubes(a: Int, b: Int): Int = sum(x => x * x * x, a, b)

*/
def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)
/*
Functions returning Functions

Let's rewrite sum as follows:
*/
def sum(f: Int => Int): (Int, Int) => Int = {
        def sumF(a: Int, b: Int): Int ={
                if(a > b) 0
                else f(a) + sumF(a + 1, b)
        }
        sumF
}

def sumInts = sum(x => x)
def sumCubes = sum(x => x * x * x)
def sumFactorials = sum(fact)

sumInts(1, 5)
sumCubes(1, 5)
sumFactorials(1, 5)

/*
sum is now a function that returns another function.

The returned function sumF applies the given function parameter f
and sums the results.
*/


/*
In the previous example, can we avoid the sumInts, sumCubes and
sumFactorials (middleman)?
*/

sum(x => x * x * x) (1, 5)


/*
Expansion of Multiple Parameter Lists

In general, a definition of a function with multiple parameter lists

    def f ( args 1 )...( args n ) = E

where n > 1, is equivalent to

    def f ( args 1 )...( args n − 1 ) = { def g ( args n ) = E ; g }

where g is a fresh identifier. Or for short:

    def f ( args 1 )...( args n − 1 ) = ( args n ⇒ E )

By repeating the process n times

    def f ( args 1 )...( args n − 1 )( args n ) = E

is shown to be equivalent to

    def f = ( args 1 ⇒ ( args 2 ⇒ ...( args n ⇒ E )...))

This style of definition and function application is called currying, named
for its instigator, Haskell Brooks Curry (1900-1982), a twentieth century
logician.

In fact, the idea goes back even further to Schönfinkel and Frege, but the
term “currying” has stuck.
*/