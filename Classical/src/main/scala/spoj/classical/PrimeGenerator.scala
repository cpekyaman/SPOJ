package spoj.classical

import scala.io.StdIn

/**
  * Created by raistlin on 8/11/2017.
  */
object PrimeGenerator extends App {
  //def isPrime(n: Long): Boolean = !(2 +: (3 to Math.sqrt(n).toInt by 2)).exists(n%_ == 0)
  def isPrime(n: Long): Boolean = !(n % 2 == 0 || Iterator.from(3, 2).takeWhile(_ <= Math.sqrt(n)).exists(n % _ == 0))

  val cases = StdIn.readInt()
  for(c <- 1 to cases) {
    val tuple = StdIn.readf2("{0,number,integer} {1,number,integer}")

    val lo = tuple._1.asInstanceOf[Long]
    val hi = tuple._2.asInstanceOf[Long]

    for(num <- lo to hi) {
      if(num > 1 && (num == 2 || isPrime(num))) {
        println(num)
      }
    }
    println
  }
}
