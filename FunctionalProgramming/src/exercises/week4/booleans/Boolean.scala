package week4.exercises.booleans

abstract class Boolean {

  def ifThenElse[T](theThen: => T, theElse: => T): T

  /*
    The && operation would take another boolean, or rather an expression of type boolean, because we know that it will
    be evaluated in a short-circuited fashion. And it would then call ifThenElse with 'x' and false. So what this
    means is that, if the Boolean itself is true, then you would return the Boolean argument 'x'. On the other hand,
    if the left hand operand, the Boolean here itself is false, then the result is immediately false.

    Si el objeto Boolean es True, devuelve el argumento 'x'.
    Si el objeto Boolean es False, devuelve siempre False.
   */
  def && (x: => Boolean): Boolean = ifThenElse(x, False)

  /*
    You'd again take the call by name parameter 'x', that's the right hand operand. And it says, if the Boolean itself
    is true, then the result is immediately true. Whereas if the boolean itself is false, then the result whatever the
    right hand side argument is.

    Si el objeto Boolean es True, devuelve siempre True.
    Si el objeto Boolean es False, devuelve el argumento 'x'.
   */
  def || (x: => Boolean): Boolean = ifThenElse(True, x)

  /*
    Unary-not would simply be implemented as ifThenElse false, true, that means if the Boolean itself is true, we return
    false. If the Boolean itself is false, we return true.

    Devuelve siempre lo contrario del objeto Boolean.
   */
  def unary_! : Boolean = ifThenElse(False, True)

  /*
    If to find out whether two Booleans are equal, we can go to the ifThenElse method. And say, if the Boolean itself is
    true, then the result is whatever the argument is. So, if the argument is also true, then the result would be true.
    If the argument is false, then the equals test would be false.

    Si el objeto Boolean es True, devuelve el argumento 'x'.
    Si el objeto Boolean es False, devuelve lo contrario del argumento 'x'.
   */
  def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)

  /*
    So we say the current Boolean is false, the argument is false then the equality test should give true. And, here we
    would have the negation operator on the argument false, so we would get true. If the argument on the other hand is
    true, the result should be false.

    Si el objeto Boolean es True, devuelve lo contrario del argumento 'x'.
    Si el objeto Boolean es False, devuelve el argumento 'x'.
   */
  def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)

  /*
    Si el objeto Boolean es True, devuelve siempre False.
    Si el objeto Boolean es False, devuelve el argumento 'x'.
   */
  def < (x: Boolean): Boolean = ifThenElse(False, x)
}

object True extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = t
}

object False extends Boolean {
  def ifThenElse[T](t: => T, e: => T) = e
}

object Main extends App{
  (False.<(True)).ifThenElse(println("true"), println("false"))
}