/*
Multiple Parameter Lists

The definition of functions that returns functions is so useful in
functional programming that there is a special sintax for it in Scala.

For example, the following definition of sum is equivalent to the one with
the nested sumF function, but shorter:
*/

def sum(f: Int => Int) (a: Int, b: Int): Int = {
  if(a > b) 0 else f(a) + sum(f) (a + 1, b)
}

sum(x => x) (1, 5)

/*
We can just combine the, two parameters lists of the outer function and the
nested function and write them one after the other.
*/