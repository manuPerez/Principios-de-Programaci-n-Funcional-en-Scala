package week4.exercises.exprpatternmatching

/**
  * Created by manuelperez on 4/07/16.
  */
trait Expr {
    def eval: Int = this match {
        case Num(n) => n
        case Var(n) => n.toInt
        case Sum(e1, e2) => e1.eval + e2.eval
        case Prod(e1, e2) => e1.eval * e2.eval
    }
}
