object Aoc1:

  def findNumbersFirstAndLast(line:String): Int =
    val nums = line.filter(_.isDigit).map(_.asDigit)
    val first = nums.head
    val last = nums.reverse.head
    s"$first$last".toInt

  private val word2Number = scala.collection.Map(
    "0" -> 0, "1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4, "5" -> 5, "6" -> 6, "7" -> 7, "8" -> 8, "9" -> 9,
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )
  def findNumbersFirstAndLastWithWords(line: String): Int =
    def go(line: String, numbers: List[Int]): List[Int] =
      line match
        case "" => numbers
        case l =>
          word2Number.find((w, _) => l.startsWith(w)) match
            case Some((w, n)) => go(l.tail, n :: numbers)
            case None => go(l.tail, numbers)

    val nums = go(line.trim, List())
    val first = nums.reverse.head
    val last = nums.head
    s"$first$last".toInt


  def p1(input: List[String]): Int =
    val numbers = input
      .map(findNumbersFirstAndLast).toList
    numbers.sum

  def p2(input: List[String]): BigInt =
    val numbers = input
      .map(findNumbersFirstAndLastWithWords).toList
    numbers.sum

