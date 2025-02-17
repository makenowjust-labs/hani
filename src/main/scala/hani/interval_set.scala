package hani

import scala.util.Random
import scala.util.hashing.MurmurHash3

/** `IntervalSet` is a set of intervals. */
final class IntervalSet[+A] private (val intervals: Seq[Interval[A]]):

  /** Checks whether the set contains the given value. */
  def contains[AA >: A](value: AA)(using Ordering[AA]): Boolean =
    intervals.exists(_.contains(value))

  /** Returns the intersection of this set and the given set. */
  infix def intersect[AA >: A](that: IntervalSet[AA])(using Ordering[AA]): IntervalSet[AA] =
    (complement union that.complement).complement

  /** Returns the union of this set and the given set. */
  infix def union[AA >: A](that: IntervalSet[AA])(using Ordering[AA]): IntervalSet[AA] =
    IntervalSet.from(intervals ++ that.intervals)

  /** Returns the complement of this set. */
  def complement[AA >: A](using Ordering[AA]): IntervalSet[AA] =
    if intervals.isEmpty then new IntervalSet(Seq(Interval(LBoundary.LUnbounded, RBoundary.RUnbounded)))
    else
      val newIntervals = Seq.newBuilder[Interval[AA]]
      var lboundary: Option[LBoundary[A]] = Some(LBoundary.LUnbounded)
      for interval <- intervals do
        for
          left <- lboundary
          right <- interval.lboundary.complement
          newInterval <- Interval.of[AA](left, right)
        do newIntervals += newInterval
        lboundary = interval.rboundary.complement
      for
        left <- lboundary
        newInterval <- Interval.of[AA](left, RBoundary.RUnbounded)
      do newIntervals += newInterval
      new IntervalSet(newIntervals.result())

  /** Checks whether the set is empty on the given number line. */
  def isEmpty[AA >: A](using NumberLine[AA]): Boolean =
    intervals.forall(_.isEmpty)

  /** Chooses a random value from the set. */
  def sample[AA >: A](random: Random)(using NumberLine[AA]): AA =
    val intervals = this.intervals.filterNot(_.isEmpty).toIndexedSeq
    if intervals.isEmpty then throw new IllegalCallerException("No sample")
    else
      val index = random.nextInt(intervals.length)
      val interval = intervals(index)
      interval.sample(random)

  override def equals(that: Any): Boolean = that match
    case that: IntervalSet[_] => intervals == that.intervals
    case _                    => false

  override def hashCode: Int =
    var hash = "IntervalSet".hashCode
    hash = MurmurHash3.mix(hash, intervals.hashCode)
    MurmurHash3.finalizeHash(hash, 1)

  override def toString: String = intervals.mkString("IntervalSet(", ", ", ")")

object IntervalSet:

  /** Returns the empty interval set. */
  def empty[A]: IntervalSet[A] = new IntervalSet(Seq.empty)

  /** Constructs an interval set from the given intervals. */
  def apply[A](intervals: Interval[A]*)(using Ordering[A]): IntervalSet[A] =
    from(intervals)

  /** Constructs an interval set from the given intervals. */
  def from[A](intervals: IterableOnce[Interval[A]])(using Ordering[A]): IntervalSet[A] =
    val sorted = intervals.iterator.toSeq.sortBy(interval => (interval.lboundary, interval.rboundary))
    if sorted.isEmpty then empty
    else
      val merged = Seq.newBuilder[Interval[A]]
      var current = sorted.head
      for interval <- sorted.tail do
        current.unionOf(interval) match
          case Some(nextCurrent) =>
            current = nextCurrent
          case None =>
            merged += current
            current = interval
      merged += current
      new IntervalSet(merged.result())
