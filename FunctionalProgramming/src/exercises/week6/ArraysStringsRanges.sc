object test {
  /*
    Arrays and Strings support the same operations
    as Seq and can implicitly be converted to sequences
    where needed.
   */
  val xs = Array(1,2,3,44)
  xs map (x => x * 2)

  val ys = "Hello World"
  ys filter (f => f.isUpper)

  /*
    Another simple kind of sequence is the range.
    It represents a sequence of evenly spaced integers.
    Three operators:
      - to (inclusive)
      - until (exclusive)
      - by (to determine step value)

    Ranges a represented as single objects with three
    fields: lower bound, upper bound, step value.
   */

  val r: Range = 1 until 5
  val s: Range = 1 to 5
  1 to 10 by 3
  6 to 1 by -2


  ys exists (c => c.isUpper)
  ys forall (c => c.isUpper)

  val pairs = List(1,2,3) zip ys
  pairs unzip

  ys flatMap (c => List('.', c))

  xs.sum
  xs.max

  (1 to 50).flatMap(x => (1 to 25).map(y => (x, y)))

  //To compute the scalar product of two vectors:
  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum

  //An alternative way to write this is with a pattern matching
  // function value.
  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map{ case (x, y) => x * y }.sum

  /*
    A number n is prime if the only divisors of n are 1 and n
    itself. What is a high-level way to write a test for primality
    of numbers? For once, value conciseness over efficiency.
   */
  def isPrime(n: Int): Boolean =
    (2 until n).forall(c => (c % n) != 0)

}

