package spoj.classical

import scala.io.StdIn

/**
  * Created by raistlin on 8/12/2017.
  */
object NextPalindrome extends App {

  case class LargeNumber(number: String) {
    private var count = 0
    private val increments = Map(
      '9' -> '0', '8' -> '9', '7' -> '8', '6' -> '7', '5' -> '6', '4' -> '5',
      '3' -> '4', '2' -> '3', '1' -> '2', '0' -> '1')

    private val inputArray = (number).toCharArray

    private def incrementChar(largeNumber: Array[Char], pos: Int): Char = {
      val result = increments(largeNumber(pos))
      largeNumber(pos) = result
      result
    }

    private def increment(largeNumber: Array[Char], pos: Int): Unit = {
      var incrPos = pos
      var result = incrementChar(largeNumber, incrPos)
      while(result == '0') {
        incrPos -= 1
        result = incrementChar(largeNumber, incrPos)
      }
    }

    private def isPalindrome(largeNumber: Array[Char]): Boolean = {
      val maxPos = (largeNumber.length / 2) - 1
      for(i <- 0 to maxPos) {
        if(largeNumber(i) != largeNumber(largeNumber.length - i - 1)) {
          return false
        }
      }

      true
    }

    def findNextPalindrome(): Unit = {
      if (isPalindrome(inputArray)) {
        increment(inputArray, inputArray.length - 1)
      }

      while (!isPalindrome(inputArray)) {
        count += 1
        increment(inputArray, inputArray.length - 1)
      }

      println(count)
      println(new String(inputArray))
    }
  }

  val cases = StdIn.readInt()
  for (t <- 1 to cases) {
    LargeNumber(StdIn.readLine().replaceFirst("^0+(?!$)", "")).findNextPalindrome()
  }
}
