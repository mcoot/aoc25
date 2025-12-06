package aoc25.day4


import aoc25.common.{CommonParsers, Grid2D, Point2D, SolutionWithParser}
import cats.parse.Parser

extension (g: Grid2D[Boolean])
  def pointAccessible(p: (Int, Int)): Boolean =
    val numAdjacent = Point2D(p._1, p._2).neighbourPoints.count {
      case np if np.inGrid(g) => g(np)
      case _ => false
    }
    g(p) && numAdjacent < 4

/**
 * A test day to check that the skeleton works
 *
 * The parts are counting the number of elements in the list, and summing them
 */
object Day4 extends SolutionWithParser[Grid2D[Boolean], Int, Int]:
  override def dayNumber: Int = 4

  override def parser: Parser[Grid2D[Boolean]] =
    CommonParsers.grid(
      CommonParsers.bool(
        CommonParsers.char('@'),
        CommonParsers.char('.')
      )
    ).map(Grid2D(_))

  override def solvePart1(input: Grid2D[Boolean]): Int =
    input.countCells { (_, p) => input.pointAccessible(p) }.toInt

  override def solvePart2(input: Grid2D[Boolean]): Int =
    var totalRemoved = 0
    var grid = input
    var didSomething = true
    while didSomething do
      didSomething = false
      grid = grid.copy()
      grid.forEachWithIndex { (_, p) =>
        if grid.pointAccessible(p) then
          grid.set(p, false)
          totalRemoved += 1
          didSomething = true
      }
    totalRemoved


@main def run(): Unit = Day4.run()


@main def test(): Unit = Day4.test()


