package week4.exercises.expr

trait Expr {
    def eval: Int = this match {
        case Num(n) => n
        case Var(n) => n.toInt
        case Sum(e1, e2) => e1.eval + e2.eval
        case Prod(e1, e2) => e1.eval * e2.eval
    }
}

case class Num(n: Int) extends Expr {}
case class Var(n: String) extends Expr {}
case class Sum(e1: Expr, e2: Expr) extends Expr {}
case class Prod(e1: Expr, e2: Expr) extends Expr {}

object expr extends App{
    def show(e: Expr): String = e match {
        case Num(n) => n.toString
        case Sum(e1, e2) => e1 match {
                                case Num(a) => a.toString
                                case Sum(a1, a2) => show(a1) + " + " + show(a2)
                                case Prod(a1, a2) => show(a1) + " * " + show(a2)
                            }
        case Prod(e1, e2) => e1 match {
                                case Num(a) => a.toString
                                case Sum(a1, a2) => "(" + show(a1) + " + " + show(a2) + ") "
                                case Prod(a1, a2) => show(a1) + " * " + show(a2)
                            }
        case Var(n) => n
    }

    println(show(Sum(Prod(Num(2), Var("x")), Var("y"))))
}