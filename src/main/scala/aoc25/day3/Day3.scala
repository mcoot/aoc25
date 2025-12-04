package aoc25.day3

import aoc25.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Numbers, Parser}

def seqToJoltage(l: Seq[Int]): Long =
  l.mkString("").toLong

def getMaxJoltage(l: Seq[Int], count: Int): Long =
  var best: List[Int] = List()
  for digit <- l do
    if best.size < count then
      best = best.appended(digit)
    else
      val potentials = (0 until count).map(i => best.take(i) ++ best.drop(i+1) ++ List(digit)).toList
      best = (best :: potentials).maxBy(seqToJoltage(_))
  seqToJoltage(best)


/**
 * A test day to check that the skeleton works
 *
 * The parts are counting the number of elements in the list, and summing them
 */
object Day3 extends SolutionWithParser[List[List[Int]], Long, Long]:
  override def dayNumber: Int = 3

  override def parser: Parser[List[List[Int]]] = CommonParsers.grid(Numbers.digit.map(_.asDigit))

  override def solvePart1(input: List[List[Int]]): Long =
    input.map(getMaxJoltage(_, 2)).sum

  override def solvePart2(input: List[List[Int]]): Long =
    input.map(getMaxJoltage(_, 12)).sum


@main def run(): Unit = Day3.run()


@main def test(): Unit = Day3.test()


