package aoc.y22


import aoc._
import aoc.Utils._
import collection.immutable.TreeMap


type File = Int
type Dir  = List[String]
type FS   = TreeMap[String, Dir | File]


object Day7 extends Exercise[List[Int]]:

  val day  = 7
  val year = 2022

  def parse(str: String) =
    val fs = str
      .split('$')
      .map(_.strip)
      .drop(2)
      .foldLeft(List("/") -> TreeMap[String, Dir | File]()) {
        case cwd -> fs -> s"cd .."   => cwd.dropRight(1) -> fs
        case cwd -> fs -> s"cd $dir" => (cwd :+ s"$dir/") -> fs
        case cwd -> fs -> ls         =>
          cwd -> (fs ++ parseLs(ls, cwd.mkString)) }._2
       
    fs.collect { case k -> (_: Dir) => dirSz(fs, k) }.toList


  def parseLs(ls: String, name: String) =
    ls.split('\n')
      .tail
      .foldLeft(TreeMap(name -> (Nil: Dir | File))) {
        case acc -> s"dir $dir"  =>
          acc.getOrElse(name, Nil) match
            case d: Dir => acc + (name -> (d :+ dir))
            case _      => acc
        case acc -> s"$sz $file" => acc + (s"$name$file" -> sz.toInt) }


  def dirSz(fs: FS, dir: String) =
    fs.collect { case k -> (v: File) if k startsWith dir => v }.sum

  def partOne(in: Input) = in.filter(_ < 100000).sum

  def partTwo(in: Input) = in.filter(_ >= (30000000 - (70000000 - in.max))).min
