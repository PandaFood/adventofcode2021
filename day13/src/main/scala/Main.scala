package day13

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

def InputFile: Array[String] = return Source.fromFile("input.txt").getLines.toArray

def PrintBoard(board: Array[Array[String]]) = board.map(x => x.map(y => if y.isBlank then "." else y)).map(x => println(x.mkString(" ")))

def FoldBoard(board: Array[Array[String]], foldInstructions: String): Array[Array[String]] = {

  var newBoard, halfBoard = board
  var foldLine = foldInstructions.substring(2).toInt

  if foldInstructions.startsWith("y") then 
    
    halfBoard = board.takeRight(foldLine).reverse
    board.take(foldLine).zipWithIndex.map((ycoord, y) => ycoord.zipWithIndex.map((dot, x) => if dot.isBlank then halfBoard(y)(x) else dot))
  else

    halfBoard = board.map(y => y.takeRight(foldLine).reverse)
    board.map(y => y.take(foldLine)).zipWithIndex.map((ycoord, y) => ycoord.zipWithIndex.map((dot, x) => if dot.isBlank then halfBoard(y)(x) else dot))
}

def CountDots(board: Array[Array[String]]): Int = board.flatten.filter(x => x == "#").length



@main def Main : Unit = {

  val input = InputFile

  var codes = input.takeWhile(x => !x.startsWith("fold along")).filter(x => !x.isEmpty).map(x => { var l = x.split(','); (l(0).toInt, l(1).toInt) })

  var instructions = input.drop(codes.length - 1).filter(x => x.startsWith("fold along"))

  val PART = sys.env.get("part") match {
    case Some(x) => x
    case _ => "part2"
  }

  println("part: " + PART)

  var answ = 0

  val maxX = codes.map(x => x._1).reduce((y, x) => math.max(y, x) )
  val maxY = codes.map(x => x._2).reduce((y, x) => math.max(y, x) )
  println(f"Max X-Y: ${maxX}-${maxY}")
  var board = Array.ofDim[String](maxY + 1, maxX + 1).map(x => x.map(y => ""))

  codes.foreach(code => {
    board(code._2)(code._1) = "#"
  })

  if PART == "part1" then

    

    val firstInstruction = instructions(0).substring(11)
    board = FoldBoard(board, firstInstruction)

    // PrintBoard(board)

    answ = CountDots(board)
  else if PART == "part2" then

    instructions.map(instruction => {
      board = FoldBoard(board, instruction.substring(11))
    })

    PrintBoard(board)

    answ = 0

  println("Answer: " + answ)

}
  


