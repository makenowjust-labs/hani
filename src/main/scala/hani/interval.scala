package hani

import scala.math.Ordering.Implicits.*
import scala.util.Random
import scala.util.hashing.MurmurHash3

import LBoundary.*
import RBoundary.*

/** `LBoundary` is a left boundary of an interval.
  *
  * A boundary is considered as an infinite interval as follows:
  *
  * ```
  * LUnbounded = (-∞, ∞)
  * LOpen(x)   = (x, ∞)
  * LClosed(x) = [x, ∞)
  * ```
  */
enum LBoundary[+A]:
  case LUnbounded
  case LOpen(point: A)
  case LClosed(point: A)

  /** Checks whether the boundary is bounded or not. */
  def isBounded: Boolean = this match
    case LUnbounded => false
    case _          => true

  /** Checks whether the boundary is closed or not. */
  def isClosed: Boolean = this match
    case LUnbounded => false
    case LOpen(_)   => false
    case LClosed(_) => true

  /** Returns the endpoint of the boundary if it is bounded.
    * Otherwise, it throws an exception.
    */
  def endpoint: A = this match
    case LUnbounded => throw new NoSuchElementException("LUnbound.endpoint")
    case LOpen(x)   => x
    case LClosed(x) => x

  /** Checks the boundary contains the given value. */
  def satisfy[AA >: A](x: AA)(using Ordering[AA]): Boolean = (this: LBoundary[AA]) match
    case LUnbounded => true
    case LOpen(l)   => l < x
    case LClosed(l) => l <= x

  /** Checks the boundary has an intersection with the given boundary. */
  def overlaps[AA >: A](rbound: RBoundary[AA])(using Ordering[AA]): Boolean =
    !isBounded || !rbound.isBounded ||
      (endpoint: AA) < rbound.endpoint ||
      (endpoint: AA) == rbound.endpoint && isClosed && rbound.isClosed

  /** Checks the boundary is connected with the given boundary. */
  def contiguous[AA >: A](rbound: RBoundary[AA])(using Ordering[AA]): Boolean =
    isBounded && rbound.isBounded && (endpoint: AA) == rbound.endpoint && isClosed != rbound.isClosed

  /** Returns the complement of the boundary if it is bounded.
    * Otherwise, it returns `None`.
    */
  def complement: Option[RBoundary[A]] = this match
    case LUnbounded => None
    case LOpen(x)   => Some(RClosed(x))
    case LClosed(x) => Some(ROpen(x))

  /** Converts the boundary to an interval. */
  def toInterval[AA >: A](using Ordering[AA]): Interval[AA] =
    Interval[AA](this, RUnbounded)

  /** Converts the boundary to an interval set. */
  def toIntervalSet[AA >: A](using Ordering[AA]): IntervalSet[AA] =
    toInterval.toIntervalSet

object LBoundary:

  given ordering[A](using A: Ordering[A]): Ordering[LBoundary[A]] with
    def compare(x: LBoundary[A], y: LBoundary[A]): Int = (x, y) match
      case (LUnbounded, LUnbounded)   => 0
      case (LUnbounded, _)            => -1
      case (_, LUnbounded)            => 1
      case (LOpen(l1), LOpen(l2))     => A.compare(l1, l2)
      case (LClosed(l1), LClosed(l2)) => A.compare(l1, l2)
      case (LOpen(l1), LClosed(l2)) =>
        val cmp = A.compare(l1, l2)
        if cmp == 0 then 1 else cmp
      case (LClosed(l1), LOpen(l2)) =>
        val cmp = A.compare(l1, l2)
        if cmp == 0 then -1 else cmp

/** `RBoundary` is a right boundary of an interval.
  *
  * A boundary is considered as an infinite interval as follows:
  *
  * ```
  * RUnbound   = (-∞, ∞)
  * ROpen(x)   = (-∞, x)
  * RClosed(x) = (-∞, x]
  * ```
  */
enum RBoundary[+A]:
  case RUnbounded
  case ROpen(point: A)
  case RClosed(point: A)

  /** Checks whether the boundary is bounded or not. */
  def isBounded: Boolean = this match
    case RUnbounded => false
    case _          => true

  /** Checks whether the boundary is closed or not. */
  def isClosed: Boolean = this match
    case RUnbounded => false
    case ROpen(_)   => false
    case RClosed(_) => true

  /** Returns the endpoint of the boundary if it is bounded.
    * Otherwise, it throws an exception.
    */
  def endpoint: A = this match
    case RUnbounded => throw new NoSuchElementException("RUnbound.endpoint")
    case ROpen(x)   => x
    case RClosed(x) => x

  /** Checks the boundary contains the given value. */
  def satisfy[AA >: A](x: AA)(using Ordering[AA]): Boolean = this match
    case RUnbounded => true
    case ROpen(r)   => x < r
    case RClosed(r) => x <= r

  /** Returns the complement of the boundary if it is bounded.
    * Otherwise, it returns `None`.
    */
  def complement: Option[LBoundary[A]] = this match
    case RUnbounded => None
    case ROpen(x)   => Some(LClosed(x))
    case RClosed(x) => Some(LOpen(x))

  /** Converts the boundary to an interval. */
  def toInterval[AA >: A](using Ordering[AA]): Interval[AA] =
    Interval[AA](LUnbounded, this)

  /** Converts the boundary to an interval set. */
  def toIntervalSet[AA >: A](using Ordering[AA]): IntervalSet[AA] =
    toInterval.toIntervalSet

object RBoundary:

  given ordering[A](using A: Ordering[A]): Ordering[RBoundary[A]] with
    def compare(x: RBoundary[A], y: RBoundary[A]): Int = (x, y) match
      case (RUnbounded, RUnbounded)   => 0
      case (RUnbounded, _)            => 1
      case (_, RUnbounded)            => -1
      case (ROpen(l1), ROpen(l2))     => A.compare(l1, l2)
      case (RClosed(l1), RClosed(l2)) => A.compare(l1, l2)
      case (ROpen(l1), RClosed(l2)) =>
        val cmp = A.compare(l1, l2)
        if cmp == 0 then -1 else cmp
      case (RClosed(l1), ROpen(l2)) =>
        val cmp = A.compare(l1, l2)
        if cmp == 0 then 1 else cmp

/** `Interval` is an non-empty interval.
  *
  * This is represented as a pair of left and right boundaries, and its value is intersection of them.
  *
  * To avoid an empty interval, the left boundary should be less than or equal to the right boundary.
  * However, this condition is incomplete if the underlying type is not continuous.
  * (e.g., `Interval(LOpen(1), ROpen(2))` is valid, but it is an empty interval on `Int`.)
  */
final class Interval[+A] private (
    val lboundary: LBoundary[A],
    val rboundary: RBoundary[A]
):

  /** Checks the interval contains the given value.
    *
    * ```
    * scala> (1 `..` 2).contains(1)
    * res0: Boolean = true
    * scala> (1 `..` 2).contains(2)
    * res1: Boolean = true
    * scala> (1 `..` 2).contains(3)
    * res2: Boolean = false
    * scala> (1 `..<` 2).contains(2)
    * res3: Boolean = false
    * ```
    */
  def contains[AA >: A](value: AA)(using Ordering[AA]): Boolean =
    lboundary.satisfy(value) && rboundary.satisfy(value)

  /** Checks the interval has an intersection with the given interval.
    *
    * ```
    * scala> (1 `..` 2).overlaps(2 `..` 3)
    * res0: Boolean = true
    * scala> (1 `..<` 2).overlaps(2 `..` 3)
    * res1: Boolean = false
    * scala> (1 `..` 2).overlaps(2 `<..` 3)
    * res2: Boolean = false
    * scala> (1 `..<` 2).overlaps(2 `<..` 3)
    * res3: Boolean = false
    */
  def overlaps[AA >: A](that: Interval[AA])(using AA: Ordering[AA]): Boolean =
    val left = Ordering[LBoundary[AA]].max(lboundary, that.lboundary)
    val right = Ordering[RBoundary[AA]].min(rboundary, that.rboundary)
    left.overlaps(right)

  /** Checks the interval is connected with the given interval.
    *
    * ```
    * scala> (1 `..` 2).contiguous(2 `..` 3)
    * res0: Boolean = false
    * scala> (1 `..<` 2).contiguous(2 `..` 3)
    * res1: Boolean = true
    * scala> (1 `..` 2).contiguous(2 `<..` 3)
    * res2: Boolean = true
    * scala> (1 `..<` 2).contiguous(2 `<..` 3)
    * res3: Boolean = false
    * ```
    */
  def contiguous[AA >: A](that: Interval[AA])(using AA: Ordering[AA]): Boolean =
    val left = Ordering[LBoundary[AA]].max(lboundary, that.lboundary)
    val right = Ordering[RBoundary[AA]].min(rboundary, that.rboundary)
    left.contiguous(right)

  /** Computes and returns the intersection of two intervals if they are overlapped.
    * Otherwise, it returns `None`.
    *
    * ```
    * scala> (1 `..` 2).intersectOf(2 `..` 3)
    * res0: Option[Interval[Int]] = Some(Interval(LClosed(2), RClosed(2)))
    * scala> (1 `..<` 2).intersectOf(2 `..` 3)
    * res1: Option[Interval[Int]] = None
    * scala> (1 `..` 2).intersectOf(2 `<..` 3)
    * res2: Option[Interval[Int]] = None
    * scala> (1 `..<` 2).intersectOf(2 `<..` 3)
    * res3: Option[Interval[Int]] = None
    * ```
    */
  def intersectOf[AA >: A](that: Interval[AA])(using AA: Ordering[AA]): Option[Interval[AA]] =
    val left = Ordering[LBoundary[AA]].max(lboundary, that.lboundary)
    val right = Ordering[RBoundary[AA]].min(rboundary, that.rboundary)
    Interval.of(left, right)

  /** Computes and returns the intersection of two intervals if they are overlapped.
    * Otherwise, it throws an exception.
    */
  infix def intersect[AA >: A](that: Interval[AA])(using AA: Ordering[AA]): Interval[AA] =
    intersectOf(that).getOrElse(throw new IllegalArgumentException("No intersection"))

  /** Computes and returns the union of two intervals if they are overlapped or contiguous.
    * Otherwise, it returns `None`.
    *
    * ```
    * scala> (1 `..` 2).unionOf(2 `..` 3)
    * res0: Option[Interval[Int]] = Some(Interval(LClosed(1), RClosed(3)))
    * scala> (1 `..<` 2).unionOf(2 `..` 3)
    * res1: Option[Interval[Int]] = Some(Interval(LClosed(1), RClosed(3)))
    * scala> (1 `..` 2).unionOf(2 `<..` 3)
    * res2: Option[Interval[Int]] = Some(Interval(LClosed(1), RClosed(3)))
    * scala> (1 `..<` 2).unionOf(2 `<..` 3)
    * res3: Option[Interval[Int]] = None
    * ```
    */
  def unionOf[AA >: A](that: Interval[AA])(using AA: Ordering[AA]): Option[Interval[AA]] =
    if overlaps(that) || contiguous(that) then
      val left = Ordering[LBoundary[AA]].min(lboundary, that.lboundary)
      val right = Ordering[RBoundary[AA]].max(rboundary, that.rboundary)
      Interval.of(left, right)
    else None

  /** Computes and returns the union of two intervals if they are overlapped or contiguous.
    * Otherwise, it throws an exception.
    */
  infix def union[AA >: A](that: Interval[AA])(using AA: Ordering[AA]): Interval[AA] =
    unionOf(that).getOrElse(throw new IllegalArgumentException("No union"))

  /** Converts the interval to an interval set. */
  def toIntervalSet[AA >: A](using Ordering[AA]): IntervalSet[AA] = IntervalSet[AA](this)

  /** Checks whether the interval is empty or not on the given number line.
    *
    * ```
    * scala> (1 `..` 1).isEmpty
    * res0: Boolean = false
    * scala> (1 `<..<` 2).isEmpty
    * res1: Boolean = true
    * scala> (1.0 `<..<` 2.0).isEmpty
    * res2: Boolean = false
    * ```
    */
  def isEmpty[AA >: A](using line: NumberLine[AA]): Boolean =
    given num: Numeric[AA] = line.numeric
    val left = lboundary match
      case LUnbounded     => line.minValue
      case LOpen(point)   => num.plus(point, line.delta)
      case LClosed(point) => point
    val right = rboundary match
      case RUnbounded     => line.maxValue
      case ROpen(point)   => num.minus(point, line.delta)
      case RClosed(point) => point
    left > right

  /** Chooses a random value from the interval. */
  def sample[AA >: A](random: Random)(using line: NumberLine[AA]): AA =
    given num: Numeric[AA] = line.numeric
    val left = lboundary match
      case LUnbounded     => line.minValue
      case LOpen(point)   => num.plus(point, line.delta)
      case LClosed(point) => point
    val right = rboundary match
      case RUnbounded     => line.maxValue
      case ROpen(point)   => num.minus(point, line.delta)
      case RClosed(point) => point
    if left > right then throw new IllegalCallerException("No sample")
    else
      val l = line.toDouble(left)
      val r = line.toDouble(right)
      val d = r - l
      val x = random.nextDouble() * d + l
      line.fromDouble(x)

  override def equals(that: Any): Boolean = that match
    case that: Interval[_] => lboundary == that.lboundary && rboundary == that.rboundary
    case _                 => false

  override def hashCode: Int =
    var hash = "Interval".hashCode
    hash = MurmurHash3.mix(hash, lboundary.##)
    hash = MurmurHash3.mix(hash, rboundary.##)
    MurmurHash3.finalizeHash(hash, 2)

  override def toString: String = s"Interval($lboundary, $rboundary)"

object Interval:

  /** Constructs an interval from the given boundaries.
    * If the boundaries are not overlapped, it throws an exception.
    */
  def apply[A](lboundary: LBoundary[A], rboundary: RBoundary[A])(using Ordering[A]): Interval[A] =
    of(lboundary, rboundary).getOrElse(throw new IllegalArgumentException("Invalid interval"))

  /** Constructs an interval from the given boundaries.
    * If the boundaries are not overlapped, it returns `None`.
    */
  def of[A](lboundary: LBoundary[A], rboundary: RBoundary[A])(using Ordering[A]): Option[Interval[A]] =
    Option.when(lboundary.overlaps(rboundary))(new Interval(lboundary, rboundary))

  /** Extracts the boundaries from the interval. */
  def unapply[A](interval: Interval[A]): Option[(LBoundary[A], RBoundary[A])] =
    Some((interval.lboundary, interval.rboundary))

  /** Constructs a closed interval from the given values. */
  def closed[A](left: A, right: A)(using Ordering[A]): Interval[A] =
    new Interval(LClosed(left), RClosed(right))

  /** Constructs an open interval from the given values. */
  def open[A](left: A, right: A)(using Ordering[A]): Interval[A] =
    new Interval(LOpen(left), ROpen(right))

  /** Constructs a closed-open interval from the given values. */
  def closedOpen[A](left: A, right: A)(using Ordering[A]): Interval[A] =
    new Interval(LClosed(left), ROpen(right))

  /** Constructs an open-closed interval from the given values. */
  def openClosed[A](left: A, right: A)(using Ordering[A]): Interval[A] =
    new Interval(LOpen(left), RClosed(right))
