/*
A number x is called fixed point of a function f if

  f(x) = x

For some functions f we can locate the fixed points by starting with an
initial estimate and then by applying f in a repetitive way.

  x, f(x), f(f(x)), f(f(f(x))), ...

until the value does not vary anymore (or the change is sufficiently small)
*/
import math.abs

object Exercise{
  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double): Boolean =
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      println(guess)
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x/2)(1)

  /*
  Return the square roots

  Here is a specification of the sqrt function:

    sqrt(x) = the number y such that y * y = x

  or, by dividing both sides of the equation with y:

    sqrt(x) = the number y such that y = x / y

  Consequently, sqrt(x) is a fixed point of the function (y => x / y)
  */

  def sqrt(x: Double) =
    fixedPoint(y => x / y)(1.0)

  /*
  but it's not working because our guess oscilates between 1 and 2
  all the time

  sqrt(2)
  */

  /*
  One way to control these oscillations that we were seeing is to
  prevent the estimation from varying too much. This is done by
  averaging succesive values of the original sequence:
   */

  def sqrt2(x: Double) =
    fixedPoint(y => (y + x / y) / 2)(x)

  sqrt2(2)

  def averageDump(f: Double => Double)(x: Double) =
    (x + f(x)) / 2

  // Exercise: Write a square root function using fixedPoint and
  //           averageDamp

  def sqrt3(x: Double) =
    fixedPoint(averageDump(y => x / y))(x)

  sqrt3(2)

}



