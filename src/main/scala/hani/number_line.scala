package hani

/** `NumberLine` represents a number line of type `A`.
  *
  * See [the Wikipedia article](https://en.wikipedia.org/wiki/Number_line).
  *
  * This abstraction is used for sampling a random value from an interval (set).
  */
trait NumberLine[A]:

  given numeric: Numeric[A]

  /** Converts a value of type `A` to a `Double` value. */
  def toDouble(a: A): Double

  /** Converts a `Double` value to a value of type `A`. */
  def fromDouble(d: Double): A

  /** Returns the smallest difference between two distinct values of type `A`. */
  def delta: A

  /** Returns the smallest value of type `A`. */
  def minValue: A

  /** Returns the largest value of type `A`. */
  def maxValue: A

object NumberLine:

  given int: NumberLine[Int] with
    given numeric: Numeric[Int] = Numeric.IntIsIntegral
    def toDouble(a: Int): Double = a.toDouble
    def fromDouble(d: Double): Int = Math.round(d).toInt
    def delta: Int = 1
    def minValue: Int = Int.MinValue
    def maxValue: Int = Int.MaxValue

  given double: NumberLine[Double] with
    given numeric: Numeric[Double] = Numeric.DoubleIsFractional
    def toDouble(a: Double): Double = a
    def fromDouble(d: Double): Double = d

    // The 64-bit floating-point numbers defined in IEEE754 have a precision of 16 digits.
    // Therefore, they are defined so that the precision of `{min, max}Value + delta`
    // is (marginally) within 16 digits.

    def delta: Double = 1e-3
    def minValue: Double = -1e12
    def maxValue: Double = 1e12
