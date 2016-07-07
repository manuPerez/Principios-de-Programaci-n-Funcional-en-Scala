package week5

import scala.collection.mutable.ListBuffer

object ImplementationList extends App{

  def Last[T](list: List[T]): T = {
    list match {
      case List() => throw new Exception("Empty list")
      case List(x) => x
      case x :: xs => Last(xs)
    }
  }

  def init[T](list: List[T]): List[T] = {
    list match {
      case List() => throw new Exception("Emty list")
      case List(x) => List()
      case x :: xs => x :: init(xs)
    }
  }
  def concat[T](listA: List[T], listB: List[T]): List[T] = {
    listA match {
      case List() => listB
      case x :: xs => x :: concat(xs, listB)
    }
  }

  def reverse[T](list: List[T]): List[T] = {
    list match {
      case List() => list
      case x :: xs => reverse(xs) ::: List(x)
    }
  }

  def removeAt[T](x: Int, list: List[T]): List[T] = list.take(x) ::: list.drop(x + 1)

  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
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
            if (lt(x2, y2)) x2 :: merge(xs2, ys)
            else y2 :: merge(xs, ys2)
        }
      val (fst, snd) = xs splitAt n
      merge(msort(fst)(lt), msort(snd)(lt))
    }
  }

  val nums = List(2, -4, 5, 7, 1)
  val numsOrdered = msort(nums)((x: Int, y: Int) => x < y)

  val fruits = List("apple", "pineapple", "orange", "banana")
  val fruitsOrdered = msort(fruits)((x: String, y: String) => x.compareTo(y) < 0)

  println(numsOrdered)
  println(fruitsOrdered)

}
