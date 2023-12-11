import scala.annotation.tailrec
import scala.math.*

final case class Pos(i: Int, j: Int)
final case class PartNumber(num: Int, start: Pos, end: Pos) {
  def isNear(p: Pos, maxI: Int, maxJ: Int): Boolean =
    val searchArea = for
      i <- max(0, start.i-1) to min(end.i + 1, maxI)
      j <- max(0, start.j-1) to min(end.j + 1, maxJ)
      if ! (i == start.i && (start.j to end.j).contains(j))
    yield Pos(i, j)
    searchArea.contains(p)
}
object Pos {
  def apply(pos: Pos): Pos = Pos(pos.i, pos.j)
}

final case class Schematic(map: Array[Array[Char]]):
  override def toString: String =
    map.map(_.toList.mkString).toList.mkString("\n")

  private var partNumberStart: Option[Pos] = None
  private var partNumberEnd: Option[Pos] = None
  private var num: Int = 0
  var partNumbers: List[PartNumber] = List.empty
  private var gears: List[Pos] = List.empty

  @tailrec
  private def read(pos: Pos): Unit =
    if  pos.i >= map.length then ()
    else
      map(pos.i)(pos.j) match
        case '.' => read(advance(pos))
        case d if d.isDigit =>
          if partNumberStart.isEmpty then
            partNumberStart = Some(pos)
            num = d.asDigit
            val newPos = advance(pos)
            if newPos.i > pos.i || !map(newPos.i)(newPos.j).isDigit then
              partNumberEnd = Some(pos)
              registerPartNumber
            read(newPos)
          else
            num = num * 10 + d.asDigit
            val newPos = advance(pos)
            if !map(newPos.i)(newPos.j).isDigit then
              partNumberEnd = Some(pos)
              registerPartNumber
            read(newPos)
        case '*' =>
          registerGear(pos)
          read(advance(pos))
        case _ => read(advance(pos))

  private def registerPartNumber =
    println(s"Part number: $num, start: $partNumberStart, end: $partNumberEnd")
    val searchArea = for
      i <- max(0, partNumberStart.get.i-1) to min(partNumberEnd.get.i + 1, map.length - 1)
      j <- max(0, partNumberStart.get.j-1) to min(partNumberEnd.get.j + 1, map.head.length - 1)
      if ! (i == partNumberStart.get.i && (partNumberStart.get.j to partNumberEnd.get.j).contains(j))
    yield Pos(i, j)

    val isPartNumber = ! searchArea.forall(p => map(p.i)(p.j) == '.')
    println(s"Is part number: $isPartNumber")
    partNumbers = if isPartNumber then PartNumber(num, partNumberStart.get, partNumberEnd.get) :: partNumbers else partNumbers
    partNumberStart = None
    partNumberEnd = None

  private def registerGear(p: Pos) = gears = p :: gears

  def gearRatios: List[Int] =
    gears.map { gp =>
      val partsNear = partNumbers.filter(_.isNear(gp, map.length - 1, map.head.length - 1))
      if partsNear.size == 2 then
        val part1 = partsNear.head
        val part2 = partsNear.last
        val ratio = part1.num * part2.num
        println(s"Gear at $gp, ratio: $ratio")
        ratio
      else 0
    }

  private def advance(pos: Pos) =
    if pos.j == map.head.length - 1
    then Pos(pos.i + 1, 0)
    else Pos(pos.i, pos.j + 1)

  read(Pos(0, 0))

object Aoc3:

  def parse(input: List[String]): Schematic =
    Schematic(input.map(l => l.toCharArray).toArray)

  def p1(input: List[String]): Int =
    val s = parse(input)
    s.partNumbers.map(_.num).sum

  def p2(input: List[String]): Int =
    val s = parse(input)
    s.gearRatios.sum
