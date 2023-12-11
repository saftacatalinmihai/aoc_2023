import scala.math.*

final case class Card(id: Int, winning: Set[Int], my: Set[Int]) {
  def winningScore: Int =
    val winningNumbersInMyCard = my.intersect(winning)
    if (winningNumbersInMyCard.nonEmpty) 1 << winningNumbersInMyCard.size - 1
    else 0

  def matching: Int = my.intersect(winning).size
}

object Aoc4:

  def parse(input: List[String]): List[Card] =
    input
      .map { lines =>
        val split = lines.split(":")
        val id = split(0).split(" ").last.toInt
        val numbers = split(1).split("\\|")
        val winning = numbers(0).trim
        val my = numbers(1).trim
        Card(
          id,
          winning.split("\\s+").map(_.toInt).toSet,
          my.split("\\s+").map(_.toInt).toSet
        )
      }

  def p1(input: List[String]): Int =
    parse(input).map{_.winningScore}.sum

  def p2(input: List[String]): Int =
    val cards = parse(input)
    cards.zipWithIndex.foldLeft(List.fill(cards.size)(1)) {
      case (cardMultipliers, (card, cardIdx)) =>
      (cardIdx + 1 to cardIdx + card.matching).foldLeft(cardMultipliers){ (multipliers, idx) =>
        multipliers.updated(idx, multipliers(idx) + cardMultipliers(cardIdx))
      }
    }.sum