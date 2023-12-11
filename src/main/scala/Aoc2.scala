import scala.math._

final case class Game(id: Int, plays: List[Play]) {
  lazy val minSet: Play =
    plays.reduce{ case (Play(r1,g1,b1), Play(r2,g2,b2)) =>
      Play(max(r1, r2), max(g1, g2), max(b1, b2))
    }

  def pow: Int = minSet.red * minSet.green * minSet.blue
}

final case class Play(red: Int, green: Int, blue: Int)

object Aoc2:

  def parseGame(input: String): Game =
    val parts = input.split(": ")
    val id = parts(0).replace("Game ", "").toInt
    val plays = parts(1).split("; ").map { play =>
      val playParts = play.split(", ").map { p =>
        val playDetails = p.split(" ")
        (playDetails(1), playDetails(0).toInt)
      }
      val playMap = playParts.toMap.withDefaultValue(0)
      Play(playMap("red"), playMap("green"), playMap("blue"))
    }.toList
    Game(id, plays)

  def isGamePossible(g: Game): Boolean =
    g.plays.forall{p =>
      p.red <= 12 && p.green <= 13 && p.blue <= 14
    }

  def p1(input: List[String]): Int =
    val games = input.map(parseGame)
    val possibleGames = games.filter(isGamePossible)
    possibleGames.map(_.id).sum

  def p2(input: List[String]): Int =
    val games = input.map(parseGame)
    games.map(_.pow).sum

