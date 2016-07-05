/*
1. Write a product function that calculates the product of the values
   of a function for the points on a given interval.
*/

def product(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 1 else f(a) * product(f)(a + 1, b)
}

product(x => x * x)(3, 4)

/*
2. Write factorial in terms of product.
*/
def fact(a: Int): Int = product(x => x)(1, a)
fact(5)

/*
3. Can you write a more general function, which generalizes both sum
   and product ?
*/

/*
what we are after is a version of map reduce.

We want to have a function, the function f would map values in the interval to new values,
and reduce, then would combine them.
*/

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if(a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

/* so, producto could be */

def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
product2(x => x * x)(3, 4)