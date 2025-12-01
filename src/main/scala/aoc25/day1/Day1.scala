package aoc25.day1

import aoc25.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


enum Direction:
  case L
  case R

  override def toString: String = this match
    case Direction.L => "L"
    case Direction.R => "R"


case class DialRotation(direction: Direction, amount: Int):
  private def sign: Int = direction match
    case Direction.L => -1
    case Direction.R => 1

  def applyWithoutMod(state: Int): Int =
    state + sign * amount

  def apply(state: Int): Int =
    Math.floorMod(applyWithoutMod(state), 100)

  override def toString: String = s"${direction}${amount}"


object Parsing:
  private def directionParser: Parser[Direction] =
    Parser.char('L').map(_ => Direction.L) |
    Parser.char('R').map(_ => Direction.R)

  private def dialRotationParser: Parser[DialRotation] =
    (directionParser ~ CommonParsers.int).map((dir, amt) => DialRotation(dir, amt))

  def inputParser: Parser[List[DialRotation]] = CommonParsers.lineSeparated(dialRotationParser)


object Day1 extends SolutionWithParser[List[DialRotation], Int, Int]:
  override def dayNumber: Int = 1

  override def parser: Parser[List[DialRotation]] = Parsing.inputParser

  override def solvePart1(input: List[DialRotation]): Int =
    var state = 50
    var zeroes = 0

    for rot <- input do
      state = rot.apply(state)
      if state == 0 then
        zeroes += 1
    zeroes

  override def solvePart2(input: List[DialRotation]): Int =
    var state = 50
    var zeroes = 0

    for rot <- input do
      val spins = Math.floorDiv(rot.amount, 100)
      zeroes += spins
      val remainingAmt = Math.floorMod(rot.amount, 100)
      val proposedState = DialRotation(rot.direction, remainingAmt).applyWithoutMod(state)
      if ((state != 0 && proposedState <= 0) || (state == 0 && proposedState <= -100)) || proposedState >= 100 then
        zeroes += 1
      state = rot.apply(state)
    zeroes

@main def run(): Unit = Day1.run()

@main def test(): Unit = Day1.test()

@main def testSpins(): Unit = Day1.test("spins")


