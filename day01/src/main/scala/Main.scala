package day1

import scala.io.Source

def InputFile: Array[Int] = return Source.fromFile("input.txt").getLines.map(_.toInt).toArray


@main def Main : Unit = {

  val input = InputFile

  val PART = sys.env.get("part") match {
    case Some(x) => x
    case _ => "part2"
  }

  println("part: " + PART)

  var answ = 0

  if PART == "part1" then
    answ = input.zipWithIndex.filter((x, i) => isPrime(x)).map((x, i) => x * i).reduce((x, y) => x + y)
  else if PART == "part2" then
    answ = input.zipWithIndex.map((x: Int, i: Int) => if (!isPrime(x) && i % 2 == 0) then x else if (!isPrime(x) && i % 2 == 1) then x * -1 else 0 ).reduce((x, y) => x + y)

  println("Answer: " + answ)

}
  
def isPrime(i: Int): Boolean = if (i <= 1) false else if (i == 2) true else !(2 until i).exists(n => i % n == 0)


