package net.aklabs.helpers

import java.text.SimpleDateFormat
import java.util.{Date, Locale, TimeZone}
import java.util.concurrent.TimeUnit

import org.pmw.tinylog.Logger

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.language.implicitConversions
/**
 * The TimeHelpers object extends the TimeHelpers. It can be imported to access all of the trait functions.
 */
object TimeHelpers extends TimeHelpers {
  /** The UTC TimeZone */
  val utc: TimeZone = TimeZone.getTimeZone("UTC")

  /** @return a standard format for the date yyyy/MM/dd */
  def dateFormatter: SimpleDateFormat = new SimpleDateFormat("yyyy/MM/dd")

  /** @return a format for the time which includes the TimeZone: HH:mm zzz*/
  def timeFormatter: SimpleDateFormat = new SimpleDateFormat("HH:mm zzz")

  /** @return today's date formatted as yyyy/MM/dd */
  def formattedDateNow: String = dateFormatter.format(new Date)

  /** @return now's time formatted as HH:mm zzz */
  def formattedTimeNow: String = timeFormatter.format(new Date)


  /** @return a formatter for internet dates (RFC822/1123) including:
   *  the day of week, the month, day of month, time and time zone */
  def internetDateFormatter: SimpleDateFormat = {
    val ret = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss 'GMT'", Locale.US)
    ret.setTimeZone(utc)
    ret
  }

  /** @return a Box[date] from a string using the internet format. */
  def boxParseInternetDate(dateString: String): Box[Date] = Helpers.tryo {
    internetDateFormatter.parse(dateString)
  }

  /** @return a date from a string using the internet format. Return the Epoch date if the parse is unsuccesful */
  def parseInternetDate(dateString: String): Date = Helpers.tryo {
    internetDateFormatter.parse(dateString)
  } openOr new Date(0L)

  /** @return a date formatted with the internet format */
  def toInternetDate(in: Date): String = internetDateFormatter.format(in)

  /** @return a date formatted with the internet format (from a number of millis) */
  def toInternetDate(in: Long): String = internetDateFormatter.format(new Date(in))

  /** @return a Full(date) or a failure if the input couldn't be translated to date (or Empty if the input is null)*/
  def toDate(in: Any): Box[Date] = {
    try {
      in match {
        case null => Empty
        case d: Date => Full(d)
        case lng: Long => Full(new Date(lng))
        case lng: Number => Full(new Date(lng.longValue))
        case Nil | Empty | None | FailureBox(_, _, _) => Empty
        case Full(v) => toDate(v)
        case Some(v) => toDate(v)
        case v :: vs => toDate(v)
        case s : String => Helpers.tryo(internetDateFormatter.parse(s)) or Helpers.tryo(dateFormatter.parse(s))
        case o => toDate(o.toString)
      }
    } catch {
      case e: Exception =>
        Logger.debug("Error parsing date "+in, e)
        FailureBox("Bad date: "+in, Full(e), Empty)
    }
  }
}

/**
 * The TimeHelpers trait provide functions to create TimeSpans (an object representing an amount of time), to manage date formats
 * or general utility functions (get the date for today, get year/month/day number,...)
 */
trait TimeHelpers {

  /** private variable allowing the access to all TimeHelpers functions from inside the TimeSpan class */
  private val outer = this

  /** transforms a long to a TimeSpanBuilder object. Usage: 3L.seconds returns a TimeSpan of 3000L millis  */
  implicit def long2DurationBuilder(in: Long): DurationBuilder = DurationBuilder(in)

  /** transforms an int to a TimeSpanBuilder object. Usage: 3.seconds returns a TimeSpan of 3000L millis  */
  implicit def int2DurationBuilder(in: Int): DurationBuilder = DurationBuilder(in)

  /** class building TimeSpans given an amount (len) and a method specify the time unit  */
  case class DurationBuilder(len: Long) {
    def nanoseconds: FiniteDuration = Duration(len, TimeUnit.NANOSECONDS)
    def nanosecond: Duration = seconds
    def milliseconds: FiniteDuration = Duration(len, TimeUnit.MILLISECONDS)
    def millisecond: Duration = milliseconds
    def seconds: FiniteDuration = Duration(len, TimeUnit.SECONDS)
    def second: Duration = seconds
    def minutes: FiniteDuration = Duration(len, TimeUnit.MINUTES)
    def minute: Duration = minutes
    def hours: FiniteDuration = Duration(len, TimeUnit.MINUTES)
    def hour: Duration = hours
    def days: FiniteDuration = Duration(len, TimeUnit.DAYS)
    def day: Duration = days
    def weeks: FiniteDuration = Duration(len * 7L, TimeUnit.DAYS)
    def week: Duration = weeks
    /*
    def months = new TimeSpan(Right(new Period().plusMonths(len.toInt)))
    def month: TimeSpan = months
    def years = new TimeSpan(Right(new Period().plusYears(len.toInt)))
    def year: TimeSpan = years
     */
  }
}