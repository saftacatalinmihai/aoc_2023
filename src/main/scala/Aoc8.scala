import scala.annotation.tailrec

object Aoc8:
  def parse(input: List[String]) =
    val pattern = input.head.split("").toList
    val rules = input.tail.tail.map(_.split(" = ").toList).map { case List(start, lr) =>
      val Array(l,r) = lr.split(", ")
      start -> (l.stripMargin('('), r.dropRight(1))
    }.toMap
    (pattern, rules)

  @tailrec
  def go(rules: scala.collection.Map[String, (String, String)], originalPattern: List[String])(start: String, p: List[String], dist: Int = 0): Int =
    if start.last == 'Z' then dist
    else
      go(rules,originalPattern)(
        if p.head == "R" then rules(start)._2 else rules(start)._1,
        if p.tail.isEmpty then originalPattern else p.tail,
        dist + 1)

  def p1(input: List[String]) =
    val (pattern, rules) = parse(input)
    go(rules, pattern)("AAA", pattern)

  def lcm(a: BigInt, b: BigInt): BigInt =
    if a > b then a / a.gcd(b) * b
    else b / a.gcd(b) * a

  def p2(input: List[String]) =
    val (pattern, rules) = parse(input)

    val f: (String, List[String], Int) => Int = go(rules, pattern)
    val found = rules.keys.toList.filter(x => x.endsWith("A")).map(x => f(x, pattern, 0))
    found.map(BigInt(_)).reduce(lcm)

