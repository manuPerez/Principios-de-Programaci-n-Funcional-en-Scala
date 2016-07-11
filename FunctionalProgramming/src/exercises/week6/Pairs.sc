object Pairs{
  val n = 7

  def isPrime(n: Int): Boolean =
    (2 until n).forall(c => (c % n) != 0)

  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j))) filter(c => isPrime(c._1 + c._2))

  //Use of For
  for{
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (for{
      (x, y) <- xs zip ys
    } yield x * y).sum
}