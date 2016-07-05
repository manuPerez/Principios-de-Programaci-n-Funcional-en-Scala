package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("abcde)".toList))

    println(countChange(4, List(1, 2)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  var res: Int = 0

  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) false
    else {
      if (chars.head.equals('(')) res += 1
      else if (chars.head.equals(')')) res -= 1
      balance(chars.tail)
    }
    if (res == 0) true else false
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else {
      if (money < 0) 0
      else {
        if (coins.isEmpty) 0 else countChange(money, coins.tail) + countChange(money - coins.head, coins)
      }
    }
  }
}