import scala.collection.immutable.NumericRange
import scala.math.*


final case class RangeMap(source: BigInt, destination: BigInt, range: Int):
  val end: BigInt = source + range - 1
  def mapRange(source: BigInt, range: Int): Option[(BigInt, Int)] =
    val end = source + range - 1
    // ----|s|----|t.s|----
    if source < this.source then
      // ----|s|----|r|----|t.s|----|t.r|
      //     \|/    \|/
      // ----|s|----|r|----
      if end < this.source then
        None
//        (source, range)
//          :: Nil

      // -----4-----8--------15------17
      // ----|s|----|t.s|----|e|----|t.e|----
      // A ---4+3
      else if end >= this.source && end <= this.end then
//        (source, (this.source - source).toInt)
          Some(this.destination, (end - this.source + 1).toInt)
//          :: Nil

      else // source + range > this.source + this.range then
//        (source, (this.source - source).toInt)
          Some(this.destination, this.range)
//          :: (this.end + 1, (end - this.end).toInt)
//          :: Nil

    else if this.source <= source && source <= this.end then
      if end <= this.end then
        Some(this.destination + (source - this.source), range)
//          :: Nil

      // --1--------4--------9--------12---
      //--|t.s|----|s|------|t.e|-----|e|

      // -----2------5--------11----------14
      // ---[t.s]---[s]-------[t.e]-------[e]
      else // source + range > this.source + this.range then
        Some(this.destination + (source - this.source), (this.end - source + 1).toInt)
//          :: (this.end + 1, (end - this.end).toInt)
//          :: Nil
    else // source > this.source + this.range then
//      (source, range) :: Nil
      None

  def getMapped(value: BigInt): Option[BigInt] =
    //    if (value >= this.source && value <= this.source + range) Some(destination + array((value - source).toInt))
    //    else None

    val x =
      if (value >= this.source && value <= this.source + range) Some(destination + (value - source))
      else None
    //    println(s"RangeMap Checking $value against $source + $range -> $destination + $range. Diff: ${value - source} => $x")
    x

final case class RangeMaps(mappings: List[RangeMap]):

  def mapRange(source: BigInt, range: Int): List[(BigInt, Int)] =
    mappings.map(x =>
      val y = x.mapRange(source, range)
      y match
        case Some(value) => value
        case None => (source, range)
    )

  def getMapped(value: BigInt): BigInt =
    //    val x = filledMap(value)
    //    x

    val mappedList = mappings.map(_.getMapped(value)).filter(_.isDefined).map(_.get)
    val mapped = if (mappedList.isEmpty) value
    else mappedList.min
    //    println(s"Map Checking $value against $mappings => $mapped")
    mapped

final case class Mappings(maps: List[RangeMaps]):

  def mapRange(source: BigInt, range: Int): List[(BigInt, Int)] =
    val ys = finalMap.foldLeft(List((source, range))){ case (ranges, mps) =>
      val y = ranges.flatMap((s, r) => mps.mapRange(s, r)).distinct
      y
    }
    ys

  def newMap: Mappings = Mappings(RangeMaps(List.empty) :: maps)

  def addMapping(mapping: RangeMap): Mappings = Mappings(maps.head.copy(mappings = mapping :: maps.head.mappings) :: maps.tail)

  def getMapped(value: BigInt): BigInt =
    finalMap.foldLeft(value) { case (v, map) =>
      //      println(s"Mapping $v -> ${map.getMapped(v)}")
      map.getMapped(v)
    }

  private lazy val finalMap: List[RangeMaps] = maps.reverse

object Mappings:
  def apply(): Mappings = Mappings(List.empty)

object Aoc5:

  // Example input:
  //
  // seeds: 79 14 55 13
  //
  //seed-to-soil map:
  //50 98 2
  //52 50 48
  //
  //soil-to-fertilizer map:
  //0 15 37
  //37 52 2
  //39 0 15
  //
  //fertilizer-to-water map:
  //49 53 8
  //0 11 42
  //42 0 7
  //57 7 4
  //
  //water-to-light map:
  //88 18 7
  //18 25 70
  //
  //light-to-temperature map:
  //45 77 23
  //81 45 19
  //68 64 13
  //
  //temperature-to-humidity map:
  //0 69 1
  //1 0 69
  //
  //humidity-to-location map:
  //60 56 37
  //56 93 4
  def parse(input: List[String]) =
    val seeds = input.head.split("seeds: ")(1).split(" ").map(BigInt(_)).toList
    (seeds, input.tail.filter(!_.isBlank).foldLeft(Mappings()) { case (m, line) =>
      if (line.contains("seed-to-soil map:") ||
        line.contains("soil-to-fertilizer") ||
        line.contains("fertilizer-to-water") ||
        line.contains("water-to-light") ||
        line.contains("light-to-temperature") ||
        line.contains("temperature-to-humidity") ||
        line.contains("humidity-to-location")
      ) m.newMap
      else
        val Array(destination, source, range) = line.split(" ")
        val rangeMap = RangeMap(BigInt(source), BigInt(destination), range.toInt)
        m.addMapping(rangeMap)
    })

  def p1(input: List[String]): BigInt =
    val (seeds, mappings) = parse(input)
    val ends = seeds.map { s =>
      val mapped = mappings.getMapped(s)
      println(
        s"Seed mapps to $s -> $mapped"
      )
      mapped
    }
    ends.min

  def p2(input: List[String]): BigInt =
//
//    assert(RangeMap(10, 100, 5).mapRange(2, 2)  == List((2, 2)))
//    assert(RangeMap(10, 100, 5).mapRange(5, 5)  == List((5, 5)))
//    assert(RangeMap(10, 100, 5).mapRange(5, 6)  == List((5, 5), (100, 1)))
//    assert(RangeMap(10, 100, 5).mapRange(9, 1)  == List((9, 1)))
//    assert(RangeMap(10, 100, 5).mapRange(9, 2)  == List((9, 1), (100, 1)))
//    assert(RangeMap(10, 100, 5).mapRange(10, 1) == List((100, 1)))
//    assert(RangeMap(10, 100, 5).mapRange(10, 2) == List((100, 2)))
//    assert(RangeMap(10, 100, 5).mapRange(10, 5) == List((100, 5)))
//    assert(RangeMap(10, 100, 5).mapRange(10, 6) == List((100, 5), (15,1)))
//    assert(RangeMap(10, 100, 5).mapRange(5, 10) == List((5, 5), (100, 5)))
//    assert(RangeMap(10, 100, 5).mapRange(5, 11) == List((5, 5), (100, 5), (15, 1)))
//    assert(RangeMap(10, 100, 5).mapRange(5, 13) == List((5, 5), (100, 5), (15, 3)))
//    assert(RangeMap(10, 100, 5).mapRange(10, 1) == List((100, 1)))
//    assert(RangeMap(10, 100, 5).mapRange(11, 1) == List((101, 1)))
//    assert(RangeMap(10, 100, 5).mapRange(11, 4) == List((101, 4)))
//    assert(RangeMap(10, 100, 5).mapRange(11, 5) == List((101, 4), (15, 1)))
//    assert(RangeMap(10, 100, 5).mapRange(11, 7) == List((101, 4), (15, 3)))
//    assert(RangeMap(10, 100, 5).mapRange(14, 5) == List((104, 1), (15, 4)))
//    assert(RangeMap(10, 100, 5).mapRange(15, 5) == List((15, 5)))

    val (seeds, mappings) = parse(input)
    //    println(mappings.getMapped(BigInt(82)))
    val ends = seeds.grouped(2)
      .flatMap { case seed_start :: len :: Nil =>
        println(s"Checking $seed_start -> ${seed_start + len}")
        seed_start to seed_start + len
        val maps = mappings.mapRange(seed_start, len.intValue)
        println(maps)
        maps
      }.toList
    println(ends)
    println(ends.min._1)
    0

//    ends.min

// List(1,2,3,4,5,6,7) => List((1,2), (3,4), (4,5), (6,7)
// val l = List(1,2,3,4,5,6,7)
//

