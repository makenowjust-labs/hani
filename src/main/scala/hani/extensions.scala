package hani

import LBoundary.*
import RBoundary.*

extension [A](x: A)

  /** Returns a closed interval from `x` to `y`. */
  def `..`(y: A)(using Ordering[A]): Interval[A] = Interval(LClosed(x), RClosed(y))

  /** Returns an open interval from `x` to `y`. */
  def `<..<`(y: A)(using Ordering[A]): Interval[A] = Interval(LOpen(x), ROpen(y))

  /** Returns a closed-open interval from `x` to `y`. */
  def `..<`(y: A)(using Ordering[A]): Interval[A] = Interval(LClosed(x), ROpen(y))

  /** Returns an open-closed interval from `x` to `y`. */
  def `<..`(y: A)(using Ordering[A]): Interval[A] = Interval(LOpen(x), RClosed(y))

extension (range: Range)

  /** Converts a `Range` to an `Interval[Int]`.
    * If its step is not 1, an `IllegalCallerException` is thrown.
    */
  def toInterval: Interval[Int] =
    if range.step == 1 then
      if range.isInclusive then range.start `..` range.end
      else range.start `..<` range.end
    else throw IllegalCallerException("Only step by 1 is supported")

  /** Converts a `Range` to an `IntervalSet[Int]`.
    * If its step is not 1, an `IllegalCallerException` is thrown.
    */
  def toIntervalSet: IntervalSet[Int] = IntervalSet(range.toInterval)
