package hani

final class ExtensionsSuite extends munit.FunSuite:

  test("extension methods"):
    assertEquals(1 `..` 3, Interval.closed(1, 3))
    assertEquals(1 `<..<` 3, Interval.open(1, 3))
    assertEquals(1 `..<` 3, Interval.closedOpen(1, 3))
    assertEquals(1 `<..` 3, Interval.openClosed(1, 3))
