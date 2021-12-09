package day9

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

def InputFile: Array[Array[Int]] = return Source.fromFile("input.txt").getLines.map(x => x.split("|").map(_.toInt).toArray ).toArray
def Clamp(value: Int, min: Int, max: Int): Int = math.max(min, math.min(value, max))

def ValueBefore(arr: Array[Int], pos: Int): Int = if (pos >= 1) then arr(pos - 1) else 9
def ValueAfter(arr: Array[Int], pos: Int): Int = if (pos < arr.length - 1) then arr(pos + 1) else 9

def ValueAbove(arr: Array[Array[Int]], posX: Int, posY: Int): Int = if (posY >= 1) then arr(posY - 1)(posX) else 9
def ValueBelow(arr: Array[Array[Int]], posX: Int, posY: Int): Int = if (posY < arr.length - 1) then arr(posY + 1)(posX) else 9

def GetNextNotVisited(arr: Array[Array[Boolean]]): (Int, Int) = arr.zipWithIndex.map((x, i) => x.zipWithIndex.map((x, j) => (x, (i, j)))).flatten.find((x, c) => x == false).getOrElse(true, (0,0))._2

    def GetValueAt(in: Array[Array[Int]], point: (Int, Int)): Int = if (point._1 >= 0 && point._1 < in.length && point._2 >= 0 && point._2 < in(0).length) then in(point._1)(point._2) else 9

    def UniqueAppend(arr: ArrayBuffer[(Int, Int)], in: ArrayBuffer[(Int, Int)]): ArrayBuffer[(Int, Int)] = in.filter(x => !arr.contains(x))

    def GetConnectedLines(in: Array[Array[Int]], visited: ArrayBuffer[(Int, Int)], startPoint: (Int, Int)): ArrayBuffer[(Int, Int)] = {
      // println(visited.mkString(" "))
      if GetValueAt(in, startPoint) == 9 || visited.contains(startPoint) then ArrayBuffer[(Int, Int)]()
      else {
        visited += startPoint
        visited ++= UniqueAppend(visited, GetConnectedLines(in, visited, (startPoint._1, startPoint._2 + 1)))
        visited ++= UniqueAppend(visited, GetConnectedLines(in, visited, (startPoint._1, startPoint._2 - 1)))
        visited ++= UniqueAppend(visited, GetConnectedLines(in, visited, (startPoint._1 + 1, startPoint._2)))
        visited ++= UniqueAppend(visited, GetConnectedLines(in, visited, (startPoint._1 - 1, startPoint._2)))
      }
    }

    def MarkBasinVisited(chart: Array[Array[Boolean]], in: Array[(Int, Int)]): Array[Array[Boolean]] =  { in.map((y, x) => chart(y)(x) = true); chart}

    def PrintVisited(visited: Array[Array[Boolean]]) = {
      visited.map(x => x.map(y => if y then "*" else "0")).map(x => println(x.mkString(" ")))
      println("_" * 20)
      println(" ")
    }

    def PrintGroups(groups: ArrayBuffer[Array[Int]]) = {
      println("Groups: ")
      groups.map(x => println(x.mkString(" ")))
      println("_" * 20)
      println(" ")
    }

@main def Main : Unit = {

  val input = InputFile

  val PART = sys.env.get("part") match {
    case Some(x) => x
    case _ => "part2"
  }

  println("part: " + PART)

  var answ = 0

  if PART == "part1" then
    // input.map(x => println(x.mkString(" ")))
    // println("_" * 20)

    var lowPoints = input.zipWithIndex.map((x, i) => x.zipWithIndex.filter((y, j) => {
    y < ValueBefore(x, j) && y < ValueAfter(x, j) && y < ValueAbove(input, j, i) && y < ValueBelow(input, j, i)
    } ).map((y, j) => y) ).flatten

    // println(lowPoints.mkString(" "))


    answ = lowPoints.map(x => x + 1).reduce((x, y) => x + y)
  else if PART == "part2" then

    var x, y = 0
    var done = false
    var visited: Array[Array[Boolean]] = input.map(x => x.map(x => if x == 9 then true else false))
    var groups = ArrayBuffer[Array[Int]]()

    // var returned = GetConnectedLines(input, ArrayBuffer[(Int, Int)](), (0, 0)).toArray
    // groups = groups.addOne(returned.map(x => GetValueAt(input, x)))
    // visited = MarkBasinVisited(visited, returned)

    // PrintVisited(visited)
    // PrintGroups(groups)

    // returned = GetConnectedLines(input, ArrayBuffer[(Int, Int)](), GetNextNotVisited(visited)).toArray
    // groups = groups.addOne(returned.map(x => GetValueAt(input, x)))
    // visited = MarkBasinVisited(visited, returned)

    // PrintVisited(visited)
    // PrintGroups(groups)

    var coords = (0, 0)
    while(!done) {
      var returned = GetConnectedLines(input, ArrayBuffer[(Int, Int)](), coords).toArray
      groups = groups.addOne(returned.map(x => GetValueAt(input, x)))
      visited = MarkBasinVisited(visited, returned)

      coords = GetNextNotVisited(visited)
      if coords == (0,0) then done = true
    }

    println("group: " + groups.map(x => x.length).sortWith(_ > _).take(3).mkString(" "))

    answ = groups.map(x => x.length).sortWith(_ > _).take(3).reduce((x, y) =>  x * y)

  println("Answer: " + answ)

}


  


