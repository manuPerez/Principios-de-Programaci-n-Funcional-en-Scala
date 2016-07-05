package week3

class Person(val name: String, val age: Int) {
  override def toString = "name: " + name + " age: " + age
}

abstract class PersonSet {
  def filter(p: Person => Boolean, acum: PersonSet): PersonSet
  def contains(p: Person): Boolean
  def union(ps: PersonSet): PersonSet
  def incl(p: Person): PersonSet
  def foreach(p: Person => Unit): Unit
}

class Empty extends PersonSet{
  override def toString = " name: Empty "

  def filter(p: Person => Boolean, acum: PersonSet) = acum

  def contains(p: Person): Boolean = false

  def union(ps: PersonSet): PersonSet = ps

  def incl(p: Person): PersonSet = new NonEmpty(p, new Empty, new Empty)

  def foreach(p: Person => Unit): Unit = ()
}

class NonEmpty(elem: Person, left: PersonSet, right: PersonSet) extends PersonSet{
  override def toString = " name: " + elem.name + left.toString + right.toString

  def filter(p: Person => Boolean, acum: PersonSet) = {
    if(p(elem)) left
    else right
  }

  def contains(p: Person): Boolean = {
    if (p.name < elem.name) left.contains(p)
    else if (p.name > elem.name) right.contains(p)
    else true
  }

  def union(ps: PersonSet): PersonSet = {
    left.union(right.union(ps)).incl(elem)
  }

  def incl(x: Person): PersonSet = {
    if(x.name < elem.name) new NonEmpty(elem, left.incl(x), right)
    else if(x.name > elem.name) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def foreach(f: Person => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

object Main2 extends App{
  val set1 = new Empty
  val set2 = set1.incl(new Person("MANU", 40))
  val set3 = set2.incl(new Person("EDU", 20))
  val c = new Person("ANGEL", 37)
  val d = new Person("PABLO", 29)
  val set4c = set3.incl(c)
  val set4d = set3.incl(d)
  val set5 = set4c.incl(d)

  println("set1  = " + set1.toString)
  println("set2  = " + set2.toString)
  println("set3  = " + set3.toString)
  println("set4c = " + set4c.toString)
  println("set4d = " + set4d.toString)
  println("set5  = " + set5.toString)
  println("")
  println("set1 contains MANU? "+set1.contains(new Person("MANU", 40)))
  println("set2 contains MANU? "+set2.contains(new Person("MANU", 40)))
  println("set4c contains PABLO? "+set4c.contains(d))
  println("set5 contains PABLO? "+set5.contains(d))
  println("set4c union set4d : "+set4c.union(set4d).toString)
}