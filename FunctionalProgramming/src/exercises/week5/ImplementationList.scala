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

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] =
        (xs, ys) match {
          case (Nil, y1 :: ys1) =>
            ys
          case (x1 :: xs1, Nil) =>
            xs
          case (x2 :: xs2, y2 :: ys2) =>
            if (x2 < y2) x2 :: merge(xs2, ys)
            else y2 :: merge(xs, ys2)
        }
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  val nums = List(2, -4, 5, 7, 1)
  val numsOrdered = msort(nums)

  println(numsOrdered)

}
