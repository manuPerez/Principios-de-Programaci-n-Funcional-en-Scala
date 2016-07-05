package week3

object Main extends App {

  trait List[T]{
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    override def toString(): String = isEmpty match {
      case false => "Empty: "+isEmpty+" head: "+head+" tail.size: "+tail.toString()
      case true => "Empty."
    }
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T]{
      def isEmpty = false
  }

  class Nil[T] extends List[T] {
      def isEmpty = true
      def head = throw new NoSuchElementException("Nil.head")
      def tail = throw new NoSuchElementException("Nil.tail")
  }

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

  object List{
//    def () = new Nil[T]
    def apply[T](elem: T): List[T] = new Cons[T](elem, new Nil[T])
    def apply[T](elem1: T, elem2: T): List[T] = new Cons[T](elem1, new Cons[T](elem2, new Nil[T]))
  }

  val num = singleton(1)
  val bool = singleton(true)

  println(num.toString())
  println(bool.toString())
}