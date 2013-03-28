package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if ( r == 0 || c == 0) 1
    else if (c >= r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def accumulateParens(chars: List[Char], count: Int): Int = {
      if (count < 0) -1
      else if (chars.isEmpty) count
      else if (chars.head == '(')
        accumulateParens(chars.tail, count + 1)
      else if (chars.head == ')')
        accumulateParens(chars.tail, count - 1)
      else
        accumulateParens(chars.tail, count)
    }

    accumulateParens(chars, 0) == 0;
  }

// chars.isEmpty: Boolean returns whether a list is empty
// chars.head: Char returns the first element of the list
// chars.tail: List[Char] returns the list without the first element

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else {
      val sortedCoins = coins.sorted
      countChange(money, sortedCoins.init) +
        countChange(money - sortedCoins.max, sortedCoins)
    }
  }
}
