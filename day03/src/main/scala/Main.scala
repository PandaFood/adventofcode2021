package day3

import scala.io.Source
def InputFile: Array[Array[Int]] = return Source.fromFile("input.txt").getLines.map(_.split("|").map(_.toInt)).toArray
def GetMostCommon(in: Array[Int]): Int = if in.reduce((x, y) => x + y) < in.length / 2f then 0 else 1
def TransposeArray(in: Array[Array[Int]], index: Int): Array[Int] = in.map(_(index))

@main def Main : Unit = 
  if sys.env.get("part").getOrElse("part1") == "part1" then
    var code = Array.ofDim[Int](InputFile(0).length).zipWithIndex.map((x,i) => GetMostCommon(TransposeArray(InputFile, i)))
    println(Integer.parseInt(code.mkString, 2) * Integer.parseInt(code.map(x => x ^ 1).mkString, 2))
  else if sys.env.get("part").getOrElse("part2") == "part2" then
    var i = 0
    var oxygen, c02 = InputFile
    while (oxygen.length > 1 || c02.length > 1) 
      if oxygen.length > 1 then oxygen = oxygen.filter(x => x(i) == GetMostCommon(TransposeArray(oxygen, i)))
      if c02.length > 1 then c02 = c02.filter(x => x(i) == (GetMostCommon(TransposeArray(c02, i)) ^ 1))
      i += 1
    println(Integer.parseInt(oxygen(0).mkString, 2) * Integer.parseInt(c02(0).mkString, 2))