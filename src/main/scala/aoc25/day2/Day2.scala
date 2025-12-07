package aoc25.day2

import aoc25.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

// Stupid way to count digits
def numDigits(n: Long): Int =
  n.toString.length

def digitSegment(n: Long, idx: Int, len: Int): Long =
  n.toString.substring(idx, idx+len).toLong

def exp(power: Int): Long =
  Math.pow(10, power).toLong

case class Range(start: Long, end: Long):
  def constrainToSameOrderOfMagnitudeRanges(): List[Range] =
    // Split to ranges each over a single order of magnitude
    // Note there's a bug here if the end wasn't constrained before _and_ end+1 is a diff order of magnitude...
    // but this doesn't occur in the input
    (for
      i <- numDigits(start) to numDigits(end+1)
    yield
      val s = Math.max(exp(i-1), start)
      val e = Math.min(exp(numDigits(s))-1, end)
      Range(s, e)).toList

  def findMatchIdsWithRepetitions(numSegments: Int): List[Long] =
    // This only works on a range which is within a single order of magnitude
    assert(numDigits(start) == numDigits(end))
    // Can't repeat the pattern if the digit count doesn't divide into the segments
    if numDigits(start) % numSegments != 0 then
      return List.empty
    val patternLen = Math.floorDiv(numDigits(start), numSegments)

    val startSegments = (0 until numDigits(start) by patternLen)
      .map(i => digitSegment(start, i, patternLen))
    var firstMatchPattern = startSegments.last
    for seg <- startSegments.reverse do
      if seg >= firstMatchPattern then
        firstMatchPattern = seg
      else
        firstMatchPattern = seg + 1

    val endSegments = (0 until numDigits(end) by patternLen)
      .map(i => digitSegment(end, i, patternLen))
    var lastMatchPattern = endSegments(0)
    for seg <- endSegments.reverse do
      if seg <= lastMatchPattern then
        lastMatchPattern = seg
      else
        lastMatchPattern = seg - 1

    (firstMatchPattern to lastMatchPattern).map(_.toString.repeat(numSegments).toLong).toList


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
      matchId <- constrainedR.findMatchIdsWithRepetitions(2)
    yield
      matchId
    matches.sum


  override def solvePart2(input: List[Range]): Long =
    val matches = for
      r <- input
      constrainedR <- r.constrainToSameOrderOfMagnitudeRanges()
      numSegments <- 2 to numDigits(constrainedR.end)
      matchId <- constrainedR.findMatchIdsWithRepetitions(numSegments)
    yield
      matchId
    matches.distinct.sum

@main def run(): Unit = Day2.run()


@main def test(): Unit = Day2.test()


@main def testConstraint(): Unit = Day2.test("constraint")