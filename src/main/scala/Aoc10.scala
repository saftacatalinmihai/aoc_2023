object Aoc10:

  enum Tile:
    case Vertical, Horizontal, N_E, N_W, S_E, S_W, Ground, Start

    override def toString: String = this match
      case Tile.Vertical => "║"
      case Tile.Horizontal => "═"
      case Tile.N_E => "╚"
      case Tile.N_W => "╝"
      case Tile.S_E => "╔"
      case Tile.S_W => "╗"
      case Tile.Ground => "░"
      case Tile.Start => "S"

  object Tile:
    def parse(s: Char): Tile = s match
      case '|' => Tile.Vertical
      case '-' => Tile.Horizontal
      case 'L' => Tile.N_E
      case 'J' => Tile.N_W
      case '7' => Tile.S_W
      case 'F' => Tile.S_E
      case 'S' => Tile.Start
      case '.' => Tile.Ground

  final case class Maze(lines: Array[Array[Tile]]):
    lazy val startPos: (Int, Int) =
      lines.zipWithIndex.flatMap { case (line, y) =>
        line.zipWithIndex.collect { case (Tile.Start, x) => (x, y) }
      }.head

    def move(oldPos: (Int, Int), crtPos: (Int, Int)): ((Int, Int), (Int, Int)) =
      val (x, y) = crtPos
      val (oldX, oldY) = oldPos
      lines(y)(x) match
        case Tile.Vertical => (crtPos, (x, if (y > oldY) y + 1 else y - 1))
        case Tile.Horizontal => (crtPos, (if (x > oldX) x + 1 else x - 1, y))
        case Tile.N_E => (crtPos, if (y > oldY) (x + 1, y) else (x, y - 1))
        case Tile.N_W => (crtPos, if (y > oldY) (x - 1, y) else (x, y - 1))
        case Tile.S_E => (crtPos, if (y < oldY) (x + 1, y) else (x, y + 1))
        case Tile.S_W => (crtPos, if (y < oldY) (x - 1, y) else (x, y + 1))
        case Tile.Ground => (oldPos, crtPos)
        case Tile.Start => (oldPos, crtPos)

    def loopLen(oldPos: (Int, Int), crtPos: (Int, Int), acc: Int = 0): Int =
      val (x,y) = crtPos
      val (oldX, oldY) = oldPos
//      println("Looking at:")
//      println((x, y))
//      println(lines(y)(x))
      if lines(y)(x) == Tile.Start then 1 + acc
      else
        val (c, n) = move(oldPos, crtPos)
        loopLen(c, n, acc + 1)

    val insideMap: scala.collection.mutable.Map[(Int, Int), Boolean] = scala.collection.mutable.Map.empty

    def loopPositions(oldPos: (Int, Int), crtPos: (Int, Int)): List[(Int, Int)] =
      val (x,y) = crtPos
      val (oldX, oldY) = oldPos
      if lines(y)(x) == Tile.Start then List(crtPos)
      else
        val (c, n) = move(oldPos, crtPos)
        crtPos :: loopPositions(c, n)



    override def toString: String =
      lines.map { _.mkString("")}.mkString("\n")

  def parse(input: List[String]) =
    Maze(input.map(line => line.map(Tile.parse).toArray).toArray)

  def p1(input: List[String]): Int =
    val maze = parse(input)
    maze.loopLen(maze.startPos,(maze.startPos._1 + 1, maze.startPos._2)) / 2

  def p2(input: List[String]): Int =
    val maze = parse(input)
    println(maze)
    println(maze.loopPositions(maze.startPos,(maze.startPos._1 + 1, maze.startPos._2)))
    0