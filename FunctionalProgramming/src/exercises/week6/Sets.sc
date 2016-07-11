object Sets{
  val fruit = Set("apple", "banana", "pear")
  val s = (1 to 6).toSet

  s map (_ + 2)
  fruit.filter(_.startsWith("app"))
  s nonEmpty

  /*
    The principal differences between sets and
    sequences are:
      1.Sets are unordered; the elements of a set
        do not have a predefined order in which
        they appear in the set
      2.sets do no t have duplicate elements
      3.The fundamental operation on sets is contains
   */

  s map (_ / 2)
  s contains 5
}