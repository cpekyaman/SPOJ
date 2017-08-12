package spoj.classical

/**
  * Created by raistlin on 8/11/2017.
  */
object LifeAndEverything extends App {
  val list = io.Source.stdin.getLines().takeWhile(_ != "42").toList
  list.foreach(println)
}
