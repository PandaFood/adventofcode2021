package day4

import scala.io.Source

def InputFile: Array[String] = return Source.fromFile("input.txt").getLines.toArray

def MarkTile(players: Array[Array[Array[(Int, Boolean)]]], in: Int) = players.map(_.map(_.map(x => if x(0) == in then (x(0), true) else x)))
def TransposeArray(in: Array[Array[(Int, Boolean)]], index: Int): Array[(Int, Boolean)] = in.map(_(index))
def IsAWinner(player: Array[Array[(Int, Boolean)]]): Boolean = (player ++ player.zipWithIndex.map((x1, i1) => x1.zipWithIndex.map((x2, i2) => TransposeArray(player, i1)(i2)))).map(x => x.map(_(1)).reduce((x, y) => x && y)).reduce((x, y) => x || y)

@main def Main : Unit = {

  val input = InputFile
  var playerTiles: Array[Array[Array[(Int, Boolean)]]]  = input.drop(2).mkString("\n").split("\n\n").map(x => x.split("\n")).map(_.map(x => x.split(' ').filter(y => y != "").map(x => (x.toInt, false))))

  if sys.env.get("part").getOrElse("part1") == "part1" then input(0).split(',').map(_.toInt).map(t => { playerTiles = MarkTile(playerTiles, t); if playerTiles.filter(x => IsAWinner(x)).length > 0 then { println("Answer:" + playerTiles.filter(x => IsAWinner(x))(0).flatten.filter((x, b) => !b).map(_._1).reduce((x, y) => x + y) * t); sys.exit }  })
  else if sys.env.get("part").getOrElse("part2") == "part2" then input(0).split(',').map(_.toInt).map(t => { playerTiles = MarkTile(playerTiles, t); if (playerTiles.length == 1) && (IsAWinner(playerTiles(0))) then { println("Answer:" + playerTiles(0).flatten.filter((x, b) => !b).map(_._1).reduce((x, y) => x + y) * t); sys.exit };  playerTiles = playerTiles.filter(x => !IsAWinner(x)) })
}