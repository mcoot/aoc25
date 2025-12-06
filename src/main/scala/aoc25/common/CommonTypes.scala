package aoc25.common

import scala.annotation.targetName
import scala.collection.mutable.ArrayBuffer

// Extension pair -> Point2D
extension (p: (Long, Long))
  def pos: Point2D = Point2D(p(0), p(1))

// 2D point, vector, or complex number type
case class Point2D(x: Long, y: Long):
  // Construct from pair
  def this(p: (Long, Long)) = this(p(0), p(1))

  // Convert to pair
  def pair: (Long, Long) = (x, y)

  def neighbourPoints: List[Point2D] =
    for
      x <- List(-1, 0, 1)
      y <- List(-1, 0, 1)
      if x != 0 || y != 0
    yield
      Point2D(this.x + x, this.y + y)

  // Vector / complex addition and subtraction
  @targetName("add")
  def +(other: Point2D): Point2D = Point2D(x + other.x, y + other.y)

  @targetName("subtract")
  def -(other: Point2D): Point2D = Point2D(x - other.x, y - other.y)

  // Scalar multiplication
  @targetName("mult")
  def *(scalar: Long): Point2D = Point2D(x * scalar, y * scalar)

  // Distance calculations
  def hdist(otherX: Long): Long = Math.abs(otherX - x)
  def hdist(other: Point2D): Long = hdist(other.x)

  def vdist(otherY: Long): Long = Math.abs(otherY - y)
  def vdist(other: Point2D): Long = vdist(other.y)

  def manhattan(other: Point2D): Long = hdist(other) + vdist(other)

  def magnitude: Double = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2))

  // Check if in bounds
  def inBounds(minBound: Point2D, maxBound: Point2D): Boolean =
    val Point2D(minX, minY) = minBound
    val Point2D(maxX, maxY) = maxBound
    x >= minX && x <= maxX && y >= minY && y <= maxY

  def inGrid[T](g: Grid2D[T]): Boolean = inBounds(Point2D(0, 0), Point2D(g.width - 1, g.height - 1))

  def constrainToBounds(minBound: Point2D, maxBound: Point2D): Point2D =
    Point2D(Math.max(minBound.x, Math.min(maxBound.x, x)), Math.max(minBound.y, Math.min(maxBound.y, y)))

  def constraintToGrid[T](g: Grid2D[T]): Point2D = constrainToBounds(Point2D(0, 0), Point2D(g.width - 1, g.height - 1))

  // Modulo the components independently, useful for wrapping
  // named floorMod rather than % because it does sane (Math.floorMod) handling of negatives
  def floorMod(modulus: Point2D): Point2D =
    Point2D(Math.floorMod(x, modulus.x), Math.floorMod(y, modulus.y))


class Grid2D[T](val grid: ArrayBuffer[ArrayBuffer[T]]):
  def this(l: List[List[T]]) = this(ArrayBuffer.from(l.map(ArrayBuffer.from(_))))

  def height: Int = grid.size
  def width: Int = if height > 0 then grid(0).size else 0

  def apply(x: Int, y: Int): T = grid(y)(x)
  def apply(p: (Int, Int)): T = apply(p._1, p._2)
  def apply(p: Point2D): T = apply((p.x.toInt, p.y.toInt))

  def set(x: Int, y: Int, v: T): Unit =
    grid(y)(x) = v
  def set(p: (Int, Int), v: T): Unit =
    set(p._1, p._2, v)
  def set(p: Point2D, v: T): Unit =
    set((p.x.toInt, p.y.toInt), v)

  def foldOverCells[B](z: B)(f: (T, (Int, Int), B) => B): B =
    grid.zipWithIndex.foldRight(z) { case ((row, y), rz) =>
      row.zipWithIndex.foldRight(rz) { case ((cell, x), b) =>
        f(cell, (x, y), b)
      }
    }

  def countCells(f: (T, (Int, Int)) => Boolean): Long =
    foldOverCells(0) { (cell, loc, count) =>
      if f(cell, loc) then
        count + 1
      else
        count
    }

  def forEachWithIndex(f: (T, (Int, Int)) => Unit): Unit =
    grid.zipWithIndex.foreach { (row, y) =>
      row.zipWithIndex.foreach { (cell, x) =>
        f(cell, (x, y))
      }
    }

  def copy(): Grid2D[T] =
    Grid2D(ArrayBuffer.from(grid.map(_.clone())))

