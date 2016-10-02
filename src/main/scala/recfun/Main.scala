package recfun

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
    if ( c == 0 || c == r) {
      1
    } else {
      pascal(c-1, r-1) + pascal(c, r-1)
    }
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    chars match {
      case Nil => true
      case ')'::tail => false
      case head::tail if head != '(' => balance(tail)
      case _ =>
        val indexOfEqualRight: Int = chars.map(_ match {case '(' => 1; case ')' => -1; case _ => 0}).foldLeft(List[Int]())(
          (result: List[Int], current: Int) => result :+ (result.lastOption.getOrElse(0) + current)).indexOf(0)

        indexOfEqualRight match {
          case -1 => false
          case x if x == chars.length =>
            balance(chars.slice(1, indexOfEqualRight))
          case _ =>
            balance(chars.slice(1, indexOfEqualRight)) && balance(chars.slice(indexOfEqualRight + 1, chars.length))
        }
    }
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    coins match {
      case Nil => 0
      case x if money < 0 => 0
      case head::Nil => money % head match {case 0 => 1; case _ => 0}
      case head::tail =>
        val maxHeadCoins: Int = Math.floor(money / head).toInt
        (0 to maxHeadCoins).foldLeft(0)((result, current) => result + countChange(money - head*current, tail))
    }
  }
}
