package day4

import scala.io.Source

def InputFile: Array[String] = return Source.fromFile("input.txt").getLines.toArray

def MarkTile(players: Array[Array[Array[(Int, Boolean)]]], in: Int) = players.map(_.map(_.map(x => if x(0) == in then (x(0), true) else x)))
def TransposeArray(in: Array[Array[(Int, Boolean)]], index: Int): Array[(Int, Boolean)] = in.map(_(index))
def IsAWinner(player: Array[Array[(Int, Boolean)]]): Boolean = 
  (player ++ player.zipWithIndex.map((x1, i1) => x1.zipWithIndex.map((x2, i2) => TransposeArray(player, i1)(i2)))).map(x => x.map(_(1)).reduce((x, y) => x && y)).reduce((x, y) => x || y)

@main def Main : Unit = {

  val input = InputFile

  val PART = sys.env.get("part") match {
    case Some(x) => x
    case _ => "part2"
  }

  var selectedTiles = input(0).split(',').map(_.toInt)
  var playerTiles: Array[Array[Array[(Int, Boolean)]]]  = input.drop(2).mkString("\n").split("\n\n").map(x => x.split("\n")).map(_.map(x => x.split(' ').filter(y => y != "").map(x => (x.toInt, false))))
  var i = 0

  if PART == "part1" then
    while (i < selectedTiles.length) {
      playerTiles = MarkTile(playerTiles, selectedTiles(i))

      var winner: Array[Array[Array[(Int, Boolean)]]] = playerTiles.filter(x => IsAWinner(x))

      if winner.length > 0 then 
        println("Answer:" + winner(0).flatten.filter((x, b) => !b).map(_._1).reduce((x, y) => x + y) * selectedTiles(i))
        sys.exit

      i += 1
    }

  else if PART == "part2" then
    while (i < selectedTiles.length) {
      playerTiles = MarkTile(playerTiles, selectedTiles(i))

      if (playerTiles.length == 1) && (IsAWinner(playerTiles(0))) then 
        println("Answer:" + playerTiles(0).flatten.filter((x, b) => !b).map(_._1).reduce((x, y) => x + y) * selectedTiles(i))
        sys.exit

      playerTiles = playerTiles.filter(x => !IsAWinner(x))

      i += 1

    }
}