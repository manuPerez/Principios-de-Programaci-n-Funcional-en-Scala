object Rationals {
  val x = new Rational(1,2)


}

class Rational(x: Int, y: Int){
  require(y != 0, "denom must be nonzero")

  // a seconnd constructor
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = {if(b == 0) a else gcd(b, a % b)}
  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g

  def less(that: Rational): Boolean = {
    ((this.sub(that)).denom < 0)
  }

  def max(that: Rational): Rational = {
    if(this.less(that)) that else this
  }

  def add(that: Rational) = {
    new Rational((numer * that.denom + denom * that.numer),
                 (denom * that.denom))
  }

  def sub(that: Rational) = {
    new Rational((numer * that.denom - denom * that.numer),
                 (denom * that.denom))

    // more elegant
    add(that.neg())
  }

  def multiply(that: Rational) = {
    new Rational((numer * that.numer), (denom * that.denom))
  }

  def divide(that: Rational) = {
    new Rational(numer * that.denom, denom * that.numer)
  }

  def neg(): Rational = new Rational(-numer, denom)

  override def toString() = numer + "/" + denom
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

val b = new Rational(1, 5)

x.less(b)
x.max(b)

//val strange = new Rational(1, 0)

//strange.add(strange)

//usigng the other constructor
new Rational(2)