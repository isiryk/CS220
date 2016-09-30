package globalclock

import java.time.{OffsetTime, ZoneOffset}
import org.scalatest.FunSuite

class GlobalClockSuite extends FunSuite {
  def clockNY(globalClock: GlobalClock) = new LocalClock(ZoneOffset.of("-4"), globalClock)
  def clockParis(globalClock: GlobalClock) = new LocalClock(ZoneOffset.of("+1"), globalClock)
  def clockMoscow(globalClock: GlobalClock) = new LocalClock(ZoneOffset.of("+3"), globalClock)
  def clockTokyo(globalClock: GlobalClock) = new LocalClock(ZoneOffset.of("+9"), globalClock)
  def clockHonolulu(globalClock: GlobalClock) = new LocalClock(ZoneOffset.of("-10"), globalClock)

  test("Create a Global Clock") {
    val g = new GlobalClock()
    assert(g.toString.endsWith("Z"), "The Global Clock's time zone must be UTC+0")
  }

  test("Create Global Clock with Specified Time") {
    val t = OffsetTime.of(1,2,3,4,ZoneOffset.of("-3"))
    val j = new GlobalClock(t)
    assert(j.toString.endsWith("Z"))
    assert(j.toString == "04:02:03.000000004Z")
  }

  test("Create a Local Clock") {
    val g = new GlobalClock()
    val c = clockNY(g)
    assert(c.toString == "00:00-04:00", "Newly initialized clock should start at 0:00, local time")
    c.update(OffsetTime.of(1,0,0,0,ZoneOffset.of("-4")))
    assert(c.toString == "01:00-04:00", "Update should change the hour")
    c.update(OffsetTime.of(8,0,0,0,ZoneOffset.of("+0")))
    assert(c.toString == "04:00-04:00", "Update should change the hour from different time zone")
  }

  test("Create observer chain") {
    val now = OffsetTime.of(0,0,0,0, ZoneOffset.of("Z"))
    val g = new GlobalClock(now)
    val ny = clockNY(g)
    val p = clockParis(g)
    val m = clockMoscow(g)
    val t = clockTokyo(g)
    val h = clockHonolulu(g)
    val testTime = now.plusSeconds(5)

    val expectedTimes = List[String](
    "00:00-04:00",
    "00:00+01:00",
    "00:00+03:00",
    "00:00+09:00",
    "00:00-10:00"
    )
    val tickExpectedTimes = List[String](
      "20:00:05-04:00",
      "01:00:05+01:00",
      "03:00:05+03:00",
      "09:00:05+09:00",
      "14:00:05-10:00"
    )

    assert(g.toString == now.toString)
    List(ny, p, m, t, h).zip(expectedTimes).foreach(ct => assert(ct._1.toString == ct._2))

    g.tick()
    assert(g.toString == testTime.toString)
    List(ny, p, m, t, h).zip(tickExpectedTimes).foreach(ct => assert(ct._1.toString == ct._2))
  }
}
