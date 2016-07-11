package week5

object ImplementationList extends App {

  def last[T](list: List[T]): T = {
    list match {
      case List() => throw new Exception("Empty list")
      case List(x) => x
      case x :: xs => last(xs)
    }
  }

  def init[T](list: List[T]): List[T] = {
    list match {
      case List() => throw new Exception("Emty list")
      case List(x) => List()
      case x :: xs => x :: init(xs)
    }
  }

//  def concat[T](listA: List[T], listB: List[T]): List[T] = {
//    listA match {
//      case List() => listB
//      case x :: xs => x :: concat(xs, listB)
//    }
//  }

  def reverse[T](list: List[T]): List[T] = {
    list match {
      case List() => list
      case x :: xs => reverse(xs) ::: List(x)
    }
  }

  def removeAt[T](x: Int, list: List[T]): List[T] = list.take(x) ::: list.drop(x + 1)

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, y1 :: ys1) =>
            ys
          case (x1 :: xs1, Nil) =>
            xs
          case (x2 :: xs2, y2 :: ys2) =>
            if (ord.lt(x2, y2)) x2 :: merge(xs2, ys)
            else y2 :: merge(xs, ys2)
        }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  /* squareList with pattern matching */
  def squareList(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y :: ys => (y * y) :: squareList(ys)
    }

  /* squareList with map */
//  abstract class List[T] {
//    def map[U](f: T => U): List[U] = this match {
//      case Nil => this
//      case x :: xs => f(x) :: xs.map(f)
//    }
//  }

  def squareListMap(xs: List[Int]): List[Int] = xs map (x => x * x)

  def posElems(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => if(y > 0) y :: posElems(ys) else posElems(ys)
  }

//  abstract class List[T] {
//    ...
//    def filter(p: T => Boolean): List[T] = this match {
//      case Nil
//      => this
//      case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
//    }
//  }

  def posElemsWithFilter(xs: List[Int]): List[Int] = xs filter (x => x > 0)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case y :: ys => val (first, rest) = xs.span(a => a == y)
                    first :: pack(rest)
  }

  def encode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil => Nil
    case y :: ys => pack(xs).flatMap(a => List((a.head, a.size)))
  }

  //another way
//  def encode[T](xs: List[T]): List[(T, Int)] =
//    pack(xs) map (ys => (ys.head, ys.length))

//  def sum(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)
//
//  def product(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) => x * y)

  //shorter
//  def sum(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)

  def product(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)

  //another way
  def sum(xs: List[Int]) = (xs foldLeft 0) (_ + _)

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys) (_ :: _)
//  (xs foldLeft ys) (_ :: _) it isn't possible replace foldRight by foldLeft because the types would not work out

  val nums = List(2, -4, 5, 7, 1)
  val numsOrdered = msort(nums)

  val fruits = List("apple", "pineapple", "orange", "banana")
  val fruitsOrdered = msort(fruits)

  println(numsOrdered)
  println(fruitsOrdered)

  val pr = squareList(nums)
  val pr2 = squareListMap(nums)

  println(pr + " " + pr2)

  println(pack(List("a", "a", "a", "b", "c", "c", "a")))

  println(encode(List("a", "a", "a", "b", "c", "c", "a")))

  println(concat(List(1,2,3,4), List(2,3,4,5,6,7)))
}

