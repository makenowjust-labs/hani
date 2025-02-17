package hani

final class IntervalSuite extends munit.FunSuite:

  val seed = 42
  val numIntervals = 100
  val numSamples = 100
  val range = 10000

  test("sample: Int"):
    val random = scala.util.Random(seed)

    for i <- 0 until numIntervals do
      val x = random.nextInt(range)
      val y = x + random.nextInt(range)
      val intervalOpt = random.nextIntervalOf(x, y)
      for interval <- intervalOpt; if !interval.isEmpty; j <- 0 until numSamples do
        val s = interval.sample(random)
        assert(interval.contains(s), s"$interval does not contain $s (seed: $seed, i: $i, j: $j)")

  test("sample: Double"):
    val random = scala.util.Random(seed)

    for i <- 0 until numIntervals do
      val x = random.nextDouble() * range
      val y = x + random.nextDouble() * range
      val intervalOpt = random.nextIntervalOf(x, y)
      for interval <- intervalOpt; if !interval.isEmpty; j <- 0 until numSamples do
        val s = interval.sample(random)
        assert(interval.contains(s), s"$interval does not contain $s (seed: $seed, i: $i, j: $j)")
