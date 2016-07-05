package week4.exercises.exprpatternmatching

/**
  * Created by manuelperez on 4/07/16.
  */
object Main extends App{
    def show(e: Expr): String = e match {
        case Num(n) => n.toString
        case Sum(e1, e2) => (e1 match {
                                case Num(a) => a.toString
                                case Sum(a1, a2) => show(a1) + " + " + show(a2)
                                case Prod(a1, a2) => show(a1) + " * " + show(a2)
                            }) + " + " + show(e2)
        case Prod(e1, e2) => (e1 match {
                                case Num(a) => a.toString
                                case Sum(a1, a2) => "(" + show(a1) + " + " + show(a2) + ")"
                                case Prod(a1, a2) => show(a1) + " * " + show(a2)
                            }) + " + " + show(e2)
        case Var(n) => n
    }

    println(show(Sum(Prod(Num(2), Var("x")), Var("y"))))
    println(show(Prod(Sum(Num(2), Var("x")), Var("y"))))
}
