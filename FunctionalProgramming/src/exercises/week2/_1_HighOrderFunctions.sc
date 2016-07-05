// take the sum of the integers between a and b

def sumInts(a: Int, b: Int): Int = {
  if(a > b) 0 else a + sumInts(a + 1, b)
}

sumInts(1, 5)

// take the sum of the cubes of all the integers between a and b

def cube(a: Int): Int = a * a * a

def sumCubes(a: Int, b: Int): Int = {
  if(a > b) 0 else cube(a) + sumCubes(a + 1, b)
}

sumCubes(1, 3)

// take the sum of the factorials of all the integers between a and b
// its not working!

def factorial(a: Int): Int = {
  if(a==0) 1 else a * factorial(a - 1)
}

def sumFactorial(a: Int, b: Int): Int = {
  if(a > b) 0 else factorial(a) + sumFactorial(a + 1, b)
}

sumFactorial(1, 4)

// summing with High Order Functions

/*
f is a parameter of the sum() function, it's not a given function.
What we have done is reuse the pattern that defines the sum() function.
We had to write that only once
*/

/*
the type f: Int => Int is the type f a function that takes an argument of
type Int and returns an argument of type Int.
So, Int => Int is the type of functions that map integers to integers.
*/

def sum(f: Int => Int, a: Int, b: Int): Int = {
  if(a > b) 0
  else f(a) + sum(f, a + 1, b)
}

def sumInts2(a: Int, b: Int): Int = sum(id, a, b)
def sumCubes2(a: Int, b: Int): Int = sum(cube, a, b)
def sumFactorial2(a: Int, b: Int): Int = sum(factorial, a, b)

def id(x: Int): Int = x