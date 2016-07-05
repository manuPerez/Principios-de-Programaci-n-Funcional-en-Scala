package week4.exercises.nat

/**
  * Created by manu on 28/06/16.
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat  {
  def isZero: Boolean = true
  def predecessor: Nat = throw new Error("Zero Not predecessor.")
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) that else throw new Exception("Negative number.")

  override def toString = "0 => Zero"
}

class Succ(n: Nat) extends Nat{
  def isZero: Boolean = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = if(that.isZero) that else n - that

  override def toString = {
    def findNumber(nat: Nat): Int =
      if (nat.isZero) 0
      else 1 + findNumber(nat.predecessor)
    val number = findNumber(this)
    String.valueOf(number) + " => " +
      ((1 to number) fold ("Zero")) ( (s,_) => "Succ(" + s + ")")
  }
}

object Main extends App{
  val cero = new Succ(Zero)
  println("cero: " + cero)

  val uno = cero.successor
  println("uno: " + uno)

  val dos = uno.successor
  println("dos: " + dos)

  val otroUno = dos.predecessor
  println("otroUno: " + otroUno)

  println("uno + dos = " + (uno + dos))


}