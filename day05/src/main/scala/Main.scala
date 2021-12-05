package day5

import scala.io.Source

def InputFile: Array[((Int, Int), (Int, Int))] = return Source.fromFile("input.txt").getLines.map(x => (x.split("->").map(y => y.split(',').map(_.replaceAll("\\s", "").toInt)).map(y =>  (y(0), y(1))))).map(x => (x(0), x(1))).toArray

def CreateStraightLine(coords: ((Int, Int), (Int, Int))): Array[(Int, Int)] = {
  var isHorizontal: Boolean = if (coords(0)(1) == coords(1)(1)) then true else false
  var halfCoords: (Int, Int) = if isHorizontal then (coords(0)(0), coords(1)(0)) else (coords(0)(1), coords(1)(1))
  var otherCord: Int = if isHorizontal then coords(0)(1) else coords(0)(0)
  
  var walkLength = math.max(halfCoords(0), halfCoords(1)) - math.min(halfCoords(0), halfCoords(1)) + 1

  Array.ofDim[(Int, Int)](walkLength).zipWithIndex.map((x, i) => 
    if isHorizontal then (math.min(halfCoords(0), halfCoords(1)) + i, otherCord) else (otherCord, math.min(halfCoords(0), halfCoords(1)) + i))
}

def CreateDiagonalLine(coords: ((Int, Int), (Int, Int))): Array[(Int, Int)] = {
  var from = if coords(0)(0) < coords(1)(0) then coords(0) else coords(1)
  var to = if coords(0)(0) < coords(1)(0) then coords(1) else coords(0)
  var walkLength = 
    if from(1) > to(1)then
      from(1) - to(1)
    else
      to(1) - from(1)

  Array.ofDim[(Int, Int)](walkLength + 1).zipWithIndex.map((x, i) => 
        if from(1) < to(1) then 
          ( from(0) + i, from(1) + i) 
        else
          ( from(0) + i, from(1) - i)
        )
}

def AddBetweenCoords(view: Array[Array[Int]], coords: Array[(Int, Int)]): Array[Array[Int]] = {coords.map((x1, x2) => view(x1)(x2) += 1 ); view}

@main def Main : Unit = {

  val input = InputFile

  val PART = sys.env.get("part") match {
    case Some(x) => x
    case _ => "part2"
  }

  println("part: " + PART)
  
  var answ = 0

  if PART == "part1" then

    var viewSize = input.map(x => Array(x(0)(0), x(0)(1), x(1)(0), x(1)(1)) ).flatten.reduce((x, y) => math.max(x, y))
    var view = Array.ofDim[Int](viewSize + 1,viewSize + 1)

    input.filter(x => (x(0)(1) == x(1)(1)) || (x(0)(0) == x(1)(0))).map(x => {
        view = AddBetweenCoords(view, CreateStraightLine(x))
    })

    answ = view.flatten.filter(x => x >= 2).length

  else if PART == "part2" then

    var viewSize = input.map(x => Array(x(0)(0), x(0)(1), x(1)(0), x(1)(1)) ).flatten.reduce((x, y) => math.max(x, y))
    var view = Array.ofDim[Int](viewSize + 1,viewSize + 1)

    input.filter(x => (x(0)(1) == x(1)(1)) || 
                      (x(0)(0) == x(1)(0) || 
                      ( math.abs(x(0)(0) - x(1)(0)) == math.abs(x(0)(1) - x(1)(1)) ) )
                      ).map(x => {
        if (x(0)(1) == x(1)(1)) || (x(0)(0) == x(1)(0)) then AddBetweenCoords(view, CreateStraightLine(x)) else AddBetweenCoords(view, CreateDiagonalLine(x))
    })

    answ = view.flatten.filter(x => x >= 2).length

  println("Answer: " + answ)

}