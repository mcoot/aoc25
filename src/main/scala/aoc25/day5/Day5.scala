package aoc25.day5

import aoc25.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}


case class Range(start: Long, end: Long):
  def contains(n: Long): Boolean = n >= start && n <= end

  def overlaps(other: Range): Boolean = contains(other.end) || contains(other.start)

  def combine(other: Range): Range =
    Range(Math.min(start, other.start), Math.max(end, other.end))

  def count: Long = end - start + 1

case class IngredientDetails(ranges: List[Range], ingredients: List[Long])


object Day5 extends SolutionWithParser[IngredientDetails, Long, Long]:
  override def dayNumber: Int = 5

  private def rangeParser: Parser[Range] = CommonParsers.pair(
    CommonParsers.long,
    CommonParsers.long,
    CommonParsers.char('-'),
  ).map((s, e) => Range(s, e))

  override def parser: Parser[IngredientDetails] =
    for
      ranges <- CommonParsers.lineSeparated(rangeParser)
      _ <- CommonParsers.blankLine
      ingredients <- CommonParsers.lineSeparated(CommonParsers.long)
    yield
      IngredientDetails(ranges, ingredients)

  override def solvePart1(input: IngredientDetails): Long =
    input.ingredients.count(ingr => input.ranges.exists(r => r.contains(ingr)))

  override def solvePart2(input: IngredientDetails): Long =
    val sorted = input.ranges
      .sortBy(_.start)

    val nonOverlappingRanges: ArrayBuffer[Range] = ArrayBuffer.empty
    for r <- sorted do
      var addToExistingAt = -1
      breakable {
        for (existingR, i) <- nonOverlappingRanges.zipWithIndex do
          if existingR.overlaps(r) then
            addToExistingAt = i
            break
      }
      if addToExistingAt == -1 then
        nonOverlappingRanges.addOne(r)
      else
        nonOverlappingRanges.update(addToExistingAt, r.combine(nonOverlappingRanges(addToExistingAt)))
    nonOverlappingRanges.map(_.count).sum


@main def run(): Unit = Day5.run()


@main def test(): Unit = Day5.test()