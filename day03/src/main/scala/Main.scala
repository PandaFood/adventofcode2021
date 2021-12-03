package day3

import scala.io.Source

def InputFile: Array[Array[Int]] = return Source.fromFile("input.txt").getLines.map(_.split("|").map(_.toInt)).toArray

@main def Main : Unit = 
  val input = InputFile

  val PART = sys.env.get("part") match {
    case Some(x) => x
    case _ => "part2"
  }

  if PART == "part1" then
    var i = 0
    var code = Array.ofDim[Int](input(0).length)

    while (i < input(0).length) {
      if input.map(_(i).toInt).reduce((x, y) => x + y) < input.length / 2 then code(i) = 0 else code(i) = 1
      i += 1
    }

    println(Integer.parseInt(code.mkString, 2) * Integer.parseInt(code.map(x => x ^ 1).mkString, 2))
  else if PART == "part2" then
    var i, filter = 0
    var oxygen, c02 = input

    while (oxygen.length > 1) {
      if oxygen.map(_(i).toInt).reduce((x, y) => x + y) < oxygen.length / 2f then filter = 0 else filter = 1
      oxygen = oxygen.filter(x => x(i) == filter)
      i += 1
    }

    i = 0

    while (c02.length > 1) {
      if c02.map(_(i).toInt).reduce((x, y) => x + y) < c02.length / 2f then filter = 1 else filter = 0
      c02 = c02.filter(x => x(i) == filter)
      i += 1
    }

    println(Integer.parseInt(oxygen(0).mkString, 2) * Integer.parseInt(c02(0).mkString, 2))
