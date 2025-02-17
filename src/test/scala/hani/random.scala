package hani

import scala.util.Random

import LBoundary.*
import RBoundary.*

extension (random: Random)
  def nextIntervalOf[A](x: A, y: A)(using Ordering[A]): Option[Interval[A]] =
    random.nextInt(23) match
      case 0 | 1 | 2    => Interval.of(LClosed(x), RClosed(y))
      case 3 | 4 | 5    => Interval.of(LClosed(x), ROpen(y))
      case 6 | 7        => Interval.of(LClosed(x), RUnbounded)
      case 8 | 9 | 10   => Interval.of(LOpen(x), RClosed(y))
      case 11 | 12 | 13 => Interval.of(LOpen(x), ROpen(y))
      case 14 | 15      => Interval.of(LOpen(x), RUnbounded)
      case 16 | 17 | 18 => Interval.of(LUnbounded, RClosed(y))
      case 19 | 20 | 21 => Interval.of(LUnbounded, ROpen(y))
      case 22           => Interval.of[A](LUnbounded, RUnbounded)
