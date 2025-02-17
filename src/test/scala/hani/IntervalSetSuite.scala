package hani

final class IntervalSetSuite extends munit.FunSuite:

  val seed = 42
  val numIntervalSets = 100
  val maxNumIntervals = 10
  val numSamples = 100
  val range = 10000

  test("sample: Int"):
    val random = scala.util.Random(seed)

    for i <- 0 until numIntervalSets do
      val numIntervals = random.nextInt(maxNumIntervals) + 1
      val intervals = Seq.newBuilder[Interval[Int]]
      for j <- 0 until numIntervals do
        val x = random.nextInt(range)
        val y = x + random.nextInt(range)
        val intervalOpt = random.nextIntervalOf(x, y)
        for interval <- intervalOpt do intervals += interval
      val intervalSet = IntervalSet.from(intervals.result())
      if !intervalSet.isEmpty then
        for j <- 0 until numSamples do
          val s = intervalSet.sample(random)
          assert(intervalSet.contains(s), s"$intervalSet does not contain $s (seed: $seed, i: $i, j: $j)")

  test("sample: Double"):
    val random = scala.util.Random(seed)

    for i <- 0 until numIntervalSets do
      val numIntervals = random.nextInt(maxNumIntervals) + 1
      val intervals = Seq.newBuilder[Interval[Double]]
      for j <- 0 until numIntervals do
        val x = random.nextDouble() * range
        val y = x + random.nextDouble() * range
        val intervalOpt = random.nextIntervalOf(x, y)
        for interval <- intervalOpt do intervals += interval
      val intervalSet = IntervalSet.from(intervals.result())
      if !intervalSet.isEmpty then
        for j <- 0 until numSamples do
          val s = intervalSet.sample(random)
          assert(intervalSet.contains(s), s"$intervalSet does not contain $s (seed: $seed, i: $i, j: $j)")
