package hani

final class NumberLineSuite extends munit.FunSuite:

  test("int: minValue + delta != minValue"):
    assertNotEquals(NumberLine.int.minValue + NumberLine.int.delta, NumberLine.int.minValue)

  test("int: maxValue - delta != maxValue"):
    assertNotEquals(NumberLine.int.maxValue - NumberLine.int.delta, NumberLine.int.maxValue)

  test("double: minValue + delta != minValue"):
    assertNotEquals(NumberLine.double.minValue + NumberLine.double.delta, NumberLine.double.minValue)

  test("double: maxValue - delta != maxValue"):
    assertNotEquals(NumberLine.double.maxValue - NumberLine.double.delta, NumberLine.double.maxValue)
