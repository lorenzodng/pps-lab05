package ex

import scala.math.sqrt

//Exercise 1
trait Vector2D:
  def x: Double
  def y: Double

  def +(other: Vector2D): Vector2D

  def -(other: Vector2D): Vector2D

  def *(scalar: Double): Vector2D

  def dot(other: Vector2D): Double

  def magnitude: Double

object Vector2D:

  def apply(x: Double, y: Double): Vector2D = Vector2DImpl(x, y)

  val zero: Vector2D = apply(0.0, 0.0)
  val i: Vector2D = apply(1.0, 0.0)
  val j: Vector2D = apply(0.0, 1.0)

  case class Vector2DImpl(x: Double, y: Double) extends Vector2D:

    def +(other: Vector2D): Vector2D = Vector2DImpl(this.x + other.x, this.y + other.y)

    def -(other: Vector2D): Vector2D = Vector2DImpl(this.x - other.x, this.y - other.y)

    def *(scalar: Double): Vector2D = Vector2DImpl(scalar * this.x, scalar * this.y)

    def dot(other: Vector2D): Double= (this.x * other.x) + (this.y * other.y)

    def magnitude: Double = sqrt((this.x * this.x) + (this.y * this.y))


@main def checkVectors(): Unit =
  import ex.Vector2D.Vector2DImpl

  val v1 = Vector2DImpl(3.0, 4.0)
  val v2 = Vector2DImpl(-1.0, 2.0)

  val sum = v1 + v2
  println(s"Sum: $sum, x: ${sum.x}, y: ${sum.y}")

  val difference = v1 - v2
  println(s"Difference: $difference, x: ${difference.x}, y: ${difference.y}")

  val scaled = v1 * 2.0
  println(s"Scaled: $scaled, x: ${scaled.x}, y: ${scaled.y}")

  val dotProduct = v1.dot(v2)
  println(s"Dot Product: $dotProduct")

  val magV1 = v1.magnitude
  println(s"Magnitude of v1: $magV1")

  val magV2 = v2.magnitude
  println(s"Magnitude of v2: $magV2")

  val multipleOps = (v1 + v2) * 3.0 - Vector2D(1.0, 1.0)
  println(s"Multiple Ops: $multipleOps, x: ${multipleOps.x}, y: ${multipleOps.y}")
