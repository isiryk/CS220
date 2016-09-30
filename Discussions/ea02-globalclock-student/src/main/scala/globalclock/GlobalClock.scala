package globalclock

/**
 * The scenario: Somewhere in the world is a global clock that keeps perfect time. Clocks around the world tick
 * along with it, using its time to make sure they have the correct time for their time zone.
 * The global clock is situated at UTC+0, or Greenwich Mean Time. All of the clocks that are synchronized with it
 * have to convert the global clock's time to their local time zone.
 * (For example, a clock in New York City is at UTC-4. When the global clock strikes 1:00 pm, it's only 9:00 am in
 * the city.)
 *
 * In this assignment you will be implementing this Global Clock system using the Observer pattern.
 * The system consists of two parts:
 *  - The GlobalClock, with extends Subject. Every 5 seconds it sends a tick to its observers.
 *  - The LocalClock, which extends Observer. It tracks the global time and displays it in the local time zone.
 *  Every time the GlobalClock ticks, it increments its time by 5 seconds and sends the updated time to its observers.
 *  We have provided you the Observer and Subject code from the lecture, slightly modified for this new problem.
 *  The specification for each class is in its documentation.
 *
 *  We will be using Java's time library for this assignment. In particular, we'll be using ZoneOffset (represents
 *  a time zone) and OffsetTime (represents time in a particular time zone):
 *  > https://docs.oracle.com/javase/8/docs/api/java/time/ZoneOffset.html
 *  > https://docs.oracle.com/javase/8/docs/api/java/time/OffsetTime.html
 *  You will find several useful methods for working with time and timezones!
 *  **YOU ABSOLUTELY MUST HAVE JAVA 1.8 INSTALLED FOR THIS ASSIGNMENT**
 *
 *  Submission: Use the included grading assistant to zip your submission and upload it to Moodle.
 *  scala -cp tools/grading-assistant.jar submission.tools.PrepareSubmission
 */

import java.time.{LocalTime, ZoneOffset, OffsetTime}

trait Subject {
  def registerObserver(o: Observer): Unit
  def removeObserver(o: Observer): Unit
  def notifyObservers(): Unit
}

trait Observer {
  def update(tick: OffsetTime)
}

/**
 * GlobalClock represents the atomic global clock. It takes the time it should start ticking from as an argument.
 * (This is very useful for testing purposes.)
 * The class should have the following features:
 *  - a default constructor that takes a time to start from (this may not be the UTC+0 timezone!)
 *  - an auxiliary constructor that takes no arguments and sets the GlobalClock to start ticking at the current time
 *  - the 3 Subject methods
 *  - a tick() method that increments the time by 5 seconds, and then notifies the observers
 *  - A toString that returns the time in the same format as an OffsetTime object's toString method (*hint hint*)
 *  - All fields in this class MUST be private. Clients will only interact through the Subject methods, tick() and
 *  toString.
 *  - The GlobalClock MUST be in the UTC+0 timezone.
 * @param startTime the time to start ticking from
 */
class GlobalClock(startTime: OffsetTime) extends Subject{

  private val diff = startTime.minusSeconds(startTime.getOffset.getTotalSeconds)
  private var time = OffsetTime.of(diff.toLocalTime, ZoneOffset.of("Z"))
  private var observers: List[Observer] = List()
  def this() ={
    this(OffsetTime.now())
  }

  def registerObserver(o: Observer): Unit = {
    observers = o :: observers
  }

  def removeObserver(o: Observer): Unit ={
    observers = observers.filterNot(_ == o)
  }

  def notifyObservers(): Unit = {
    observers.foreach(_.update(time))
  }

  def tick() = {
    time = time.plusSeconds(5)
    notifyObservers()
  }

  override def toString(): String = time.toString()

}


/**
 * LocalClock represents a clock that is beholden to a timezone. It takes the timezone it is in and the GlobalClock
 * that it observes. It extends Observer.
 * When the LocalClock is initialized, it must have time 00:00:00 (midnight) of the given time zone. It also must
 * register itself with the subject, of course.
 * When update() is called, the LocalClock will receive a new time from the GlobalClock. It must convert this time
 * to its own timezone and update its own time.
 * The toString returns the time in the same format as an OffsetTime object's toString method. THE TESTS DEPEND ON THIS.
 * (Don't overthink it!)
 * @param timeZone the timezone that this clock works in
 * @param globalClock the GlobalClock this clock observes.
 */
class LocalClock(val timeZone: ZoneOffset, globalClock: GlobalClock) extends Observer {
  globalClock.registerObserver(this)
  private var time = OffsetTime.of(0,0,0,0,timeZone)
  def update(tick: OffsetTime): Unit = {
    time = tick.withOffsetSameInstant(timeZone)
  }
  override def toString(): String = {
    time.toString()
  }
}
