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
   * This function computed Pascal's triangle elements recursively
   */
    def pascal(c: Int, r: Int): Int = {
      if(c < 0 || r < 0) throw new IllegalArgumentException("Column and row numbers can't be negative")
      else
        if(c == 0 || c == r) 1
        else pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * This function defines whether a line given through a list of chars has balanced parenthesis.
    * It means that the number of closed parenthesis should be equal to the number of opened ones.
   */
    def balance(chars: List[Char]): Boolean = {
      def balanced(chars: List[Char], open: Int): Boolean ={
        if(chars.isEmpty) open == 0
        else if(chars.head == '(')
          balanced(chars.tail, open+1)
        else if(chars.head == ')')
          open > 0 && balanced(chars.tail, open-1)
        else balanced(chars.tail, open)
      }
      balanced(chars, 0)
    }
  
  /**
   * This method counts by how money ways it is possible to give a change for the given amount of money
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if ((money < 0) || coins.isEmpty) 0
      else {
        if (money == 0) 1
        else {
          countChange(money - coins.head, coins) + countChange(money, coins.tail)
        }
    }
  }
}
