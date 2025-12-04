package aoc25.day3


import aoc25.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Numbers, Parser}

import scala.util.control.Breaks._
import scala.collection.mutable.ArrayBuffer

def seqToJoltage(l: Seq[Int]): Int =
  l.mkString("").toInt


/**
 * A test day to check that the skeleton works
 *
 * The parts are counting the number of elements in the list, and summing them
 */
object Day3 extends SolutionWithParser[List[List[Int]], Int, Int]:
  override def dayNumber: Int = 3

  override def parser: Parser[List[List[Int]]] = CommonParsers.grid(Numbers.digit.map(_.asDigit))

  override def solvePart1(input: List[List[Int]]): Int =
    input.map { row =>
      var best: List[Int] = List()
      for digit <- row do
        if best.size < 2 then
          best = best.appended(digit)
        else
          val potentialA = best.drop(1).appended(digit)
          val potentialB = best.take(1).appended(digit)
          best = List(best, potentialA, potentialB).maxBy(seqToJoltage(_))
      best
    }.map(seqToJoltage(_)).sum

  override def solvePart2(input: List[List[Int]]): Int = 0


@main def run(): Unit = Day3.run()


@main def test(): Unit = Day3.test()


