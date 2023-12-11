object Main:

  // Read all lines from file and return a List of String
  def readInput(file: String): List[String] =
    val s = io.Source.fromFile(file)
    val lines = s.getLines().toList
    s.close()
    lines

  def main(args: Array[String]): Unit =
    val input = readInput("aoc_day10_2_simple.txt")
//    val input = readInput("aoc_day10.txt")
    println(Aoc10.p2(input))

