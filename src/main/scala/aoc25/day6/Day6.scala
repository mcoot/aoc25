package aoc25.day6

import aoc25.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


enum Operator:
  case Add
  case Mult

  def apply(l: Long, r: Long): Long = this match
    case Add => l + r
    case Mult => l * r


case class Problem(operator: Operator, operands: List[Long]):
  def execute(): Long = operands.reduce(operator.apply)


object Day6 extends SolutionWithParser[List[Problem], Long, Long]:
  override def dayNumber: Int = 6

  private def operatorParser: Parser[Operator] =
    CommonParsers.char('+').map(_ => Operator.Add) |
      CommonParsers.char('*').map(_ => Operator.Mult)

  override def parser: Parser[List[Problem]] =
    for
      operandColumns <- CommonParsers.lineSeparated(
        CommonParsers.withTrimmedSpaces(
          CommonParsers.spaceSeparated(CommonParsers.long)
        )
      )
      _ <- CommonParsers.newLine
      operators <- CommonParsers.spaceSeparated(operatorParser)
    yield
      operandColumns.transpose.zip(operators).map((operands, operator) => Problem(operator, operands))

  override def solvePart1(input: List[Problem]): Long =
    input.map(_.execute()).sum

  override def solvePart2(input: List[Problem]): Long =
    0


@main def run(): Unit = Day6.run()


@main def test(): Unit = Day6.test()