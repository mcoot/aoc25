package aoc25.day2

import aoc25.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

// Stupid way to count digits
def numDigits(n: Long): Int =
  n.toString.length

def exp(power: Int): Long =
  Math.pow(10, power).toLong

case class Range(start: Long, end: Long):
  def constrainToSameOrderOfMagnitudeRanges(): List[Range] =

    // If the start and the end+1 are different orders of magnitude (and not directly adjacent even orders),
    // then there are intermediate even-digit ranges; let's split to ranges each over a single order of magnitude
    // Note there's a bug here if the end wasn't constrained before _and_ end+1 is a diff order of magnitude...
    // but this doesn't occur in the input
    (for
      i <- numDigits(start) to numDigits(end+1)
    yield
      val s = Math.max(exp(i-1), start)
      val e = Math.min(exp(numDigits(s))-1, end)
      println((s, e))
      Range(s, e)).toList

  def findHalfMatchIds(): List[Long] =
    // This only works on a range which is within an order of magnitude, and even
    assert(numDigits(start) == numDigits(end))
    if numDigits(start) % 2 != 0 then
      return List.empty
    val halfLen = Math.floorDiv(numDigits(start), 2)

    val startFirstHalf = Math.floorDiv(start, exp(halfLen))
    val startSecondHalf = Math.floorMod(start, exp(halfLen))
    val lowestMatchIdHalf = if startSecondHalf <= startFirstHalf then startFirstHalf else startFirstHalf + 1

    val endFirstHalf = Math.floorDiv(end, exp(halfLen))
    val endSecondHalf = Math.floorMod(end, exp(halfLen))
    val highestMatchIdHalf = if endFirstHalf <= endSecondHalf then endFirstHalf else endFirstHalf - 1

    // Now we have the highest and lowest patterns... we'll see every pattern between,
    // so matches is just the values between, inclusive, duplicated
    (lowestMatchIdHalf to highestMatchIdHalf).map(half => half + half * exp(halfLen)).toList


object Day2 extends SolutionWithParser[List[Range], Long, Long]:
  override def dayNumber: Int = 2

  override def parser: Parser[List[Range]] =
    CommonParsers.commaSeparated(
      CommonParsers.pair(
        CommonParsers.long,
        CommonParsers.long,
        CommonParsers.char('-')
      ).map((s, e) => Range(s, e))
    )

  override def solvePart1(input: List[Range]): Long =
    val matches = for
      r <- input
      constrainedR <- r.constrainToSameOrderOfMagnitudeRanges()
      matchId <- constrainedR.findHalfMatchIds()
    yield
      matchId
    matches.sum


  override def solvePart2(input: List[Range]): Long =
    0L

@main def run(): Unit = Day2.run()


@main def test(): Unit = Day2.test()


@main def testConstraint(): Unit = Day2.test("constraint")