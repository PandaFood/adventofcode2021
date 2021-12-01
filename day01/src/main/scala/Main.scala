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
    input.reduce((x,y) => {
    if y > x then answ += 1
    y })

  else if PART == "part2" then
    input.zipWithIndex.map((x: Int, i: Int) => x + input((i + 1).min((input.length-1))) + input((i + 2).min((input.length-1))) ).take(input.length-2).reduce((x,y) => { if y > x then answ += 1; y })
  
  println("Answer: " + answ)

}
  


