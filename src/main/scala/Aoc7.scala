import scala.collection.immutable

object Aoc7:

  enum Type extends Enum[Type]:
    case HighCard, OnePair, TwoPairs, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind

  final case class Hand(cards: List[Card], bid: Int, withJokers: Boolean = false):
    override def toString: String = s"Hand(${cards.mkString(",")}, $bid)"

    def jokers: Int = cards.count(_ == Card(11))

    val `type`: Type =
      val cs =
        if withJokers then
          val csNoJokers = cards
            .groupBy(_.n)
            .map { case (n, v) => n -> v.length }
            .toList.sorted.reverse
            .filter(_._1 != 11)
          val max = if csNoJokers.isEmpty then 0 else csNoJokers.map((n, l) => (l, n)).max._2
          val cardsWithReplacements = cards.map(c => if c.n == 11 then Card(max) else c)
          cardsWithReplacements
            .groupBy(_.n)
            .map { case (_, v) => v.length }
            .toList.sorted.reverse
        else
          cards.groupBy(_.n).map { case (_, v) => v.length }.toList.sorted.reverse

      cs match
        case 1 :: _ => Type.HighCard
        case 2 :: 2 :: _ => Type.TwoPairs
        case 2 :: _ => Type.OnePair
        case 3 :: 2 :: Nil => Type.FullHouse
        case 3 :: _ => Type.ThreeOfAKind
        case 4 :: _ => Type.FourOfAKind
        case List(5) => Type.FiveOfAKind
        case Nil => Type.FiveOfAKind

    def >(that: Hand): Boolean =
      if this.`type` == that.`type` then
        def cmp(thisCards: List[Card], thatCards: List[Card]): Boolean =
          thisCards match
            case Nil => false
            case h1 :: t1 =>
              if h1 > thatCards.head then true
              else if thatCards.head > h1 then false
              else cmp(t1, thatCards.tail)

        if this.withJokers then
          cmp(
            this.cards.map(c => if c.n == 11 then Card(1) else c),
            that.cards.map(c => if c.n == 11 then Card(1) else c)
          )
        else
          cmp(this.cards, that.cards)
      else
        this.`type`.compareTo(that.`type`) > 0

  object Hand:
    def apply(line: String): Hand =
      val Array(cards, bid) = line.split(" ")
      Hand(cards.map(Card(_)).toList, bid.toInt)

    def withJokers(line: String): Hand =
      val Array(cards, bid) = line.split(" ")
      Hand(cards.map(Card(_)).toList, bid.toInt, true)


  final case class Card(n: Int):
    override def toString: String = n match
      case 14 => "A"
      case 13 => "K"
      case 12 => "Q"
      case 11 => "J"
      case 10 => "T"
      case _ => n.toString

    def >(that: Card): Boolean = n > that.n

  object Card:
    def apply(c: Char): Card = Card(
      c match
        case 'A' => 14
        case 'K' => 13
        case 'Q' => 12
        case 'J' => 11
        case 'T' => 10
        case _ => c.asDigit
    )

  def p1(input: List[String]) =
    val hands = input.map(Hand(_))
    hands.sortWith(_ > _).reverse.zipWithIndex.map((h, i) => h.bid * (i + 1)).sum

  def p2(input: List[String]) =
    val hands = input.map(Hand.withJokers)
    hands.sortWith(_ > _).reverse.zipWithIndex.map((h, i) => h.bid * (i + 1)).sum