object Aoc9:

  //0 3 6 9 12 15
  //1 3 6 10 15 21
  //10 13 16 21 30 45
  def parse(input: List[String]): List[List[Int]] =
    input.map(_.split(" ").map(_.toInt).toList)

  private def derivative(input: List[Int]): List[Int]=
    input.zip(input.tail).map((a, b) => b - a)

  private def derivateTillNoChange(list: List[Int], accFn: (List[Int], Int) => Int): Int =
    if list.forall(_ == 0) then 0
    else accFn(list, derivateTillNoChange(derivative(list), accFn))

  def p1(input: List[String]): Int =
    parse(input).map(derivateTillNoChange(_, _.last + _)).sum

  def p2(input: List[String]): Int =
    parse(input).map(derivateTillNoChange(_, _.head - _)).sum