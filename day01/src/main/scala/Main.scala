package day1

import scala.io.Source

val PART =  sys.env("part")

def InputFile: Array[Int] = return Source.fromFile("input.txt").getLines.map(_.toInt).toArray


@main def Main : Unit = {

  val input = InputFile
  // val input: Array[Int] = Array(12, 14, 1969, 100756)

  //input.map(x => println(x))

  println("part: " + PART)

  if PART == "part1" then

    val answ = input.map(x => ((x/3)-2)).reduce((x, y) => x + y)
    println("Answer: " + answ)

}
  
