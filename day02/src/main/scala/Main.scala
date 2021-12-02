package day1

import scala.io.Source

def InputFile: Array[(String, Int)] = return Source.fromFile("input.txt").getLines.map(x => (x.split(" ")(0), x.split(" ")(1).toInt )).toArray


@main def Main : Unit = {

  val input = InputFile

  val PART = sys.env.get("part") match {
    case Some(x) => x
    case _ => "part2"
  }

  println("part: " + PART)

  var answ = 0

  if PART == "part1" then
    var speed = input.filter(x => x(0) == "forward").map(_(1)).reduce((x , y) => { x + y })
    var depth = input.filter(x => x(0) != "forward").reduce((x , y) => { if y(0) == "down" then (x(0), x(1) + y(1)) else (x(0), x(1) - y(1)) })(1)
    answ = speed * depth
  else if PART == "part2" then
    var aim, speed, depth = 0
    for (order <- input) {
      if order(0) == "forward" then 
        speed = speed + order(1)
        depth = depth + (aim * order(1))
      else if order(0) == "up" then
        aim = aim - order(1)
      else if order(0) == "down" then
        aim = aim + order(1)
    }
    answ = speed * depth

  println("Answer: " + answ)

}
  


