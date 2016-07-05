def factorial(x: Int): Int = {
  if(x == 0) 1 else x * factorial(x - 1)
}

factorial(4)

//hacer la función Tail Recursion
def factorial2(x: Int): Int = {
  def loop(acum: Int, n: Int): Int =
    if(n == 0) acum
    else loop(acum *n, n - 1)
  loop(1, x)
}

factorial2(4)