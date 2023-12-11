object Aoc6:

  final case class Boat(buttonPressTime: Int):
    def distance(timeAllowed: BigInt): BigInt =
      val travelTime = timeAllowed - buttonPressTime
      buttonPressTime * travelTime

  final case class Race(timeAllowed: Int, record: BigInt)

  def parse1(input: List[String]): List[Race] =
    val times = input.head.split("Time:\\s+")(1).split("\\s+").toList.map(_.toInt)
    val distances = input.last.split("Distance:\\s+")(1).split("\\s+").toList.map(BigInt(_))
    times.zip(distances).map(Race.apply)

  def p1(input: List[String]) =
    val rs = parse1(input)
    val beatsRecord = rs.map(r =>
      (1 to r.timeAllowed).map(Boat.apply).map(_.distance(r.timeAllowed)).count(_ > r.record)
    )
    beatsRecord.product

  def parse2(input: List[String]): Race =
    val time = input.head.split("Time:\\s+")(1).split("\\s+").toList.mkString("").toInt
    val distance = BigInt(input.last.split("Distance:\\s+")(1).split("\\s+").toList.mkString(""))
    Race(time, distance)

  def p2(input: List[String]) =
    val r = parse2(input)
    (1 to r.timeAllowed).map(Boat.apply).map(_.distance(r.timeAllowed)).count(_ > r.record)
