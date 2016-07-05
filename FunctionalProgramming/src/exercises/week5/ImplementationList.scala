package week5

import scala.collection.mutable.ListBuffer

class ImplementationList {

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
      case List(x) => _
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
}
