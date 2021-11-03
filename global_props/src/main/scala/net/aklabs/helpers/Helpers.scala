package net.aklabs.helpers

import java.lang.{StringBuilder => GoodSB}
import java.security.SecureRandom
import java.util.Random
import java.util.concurrent.atomic.AtomicLong
import StringHelpers._

object Helpers {

  /**
   * Wraps a "try" block around the function f. If f throws
   * an exception with its class in the 'ignore' list or if 'ignore' is
   * null or an empty list, ignore the exception and return None.
   *
   * @param ignore - a list of exception classes to ignore. A thrown exception will be ignored if it is assignable from one of
   * the exception classes in the list
   * @param onError - an optional callback function that will use the thrown exception as a parameter
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: List[Class[_]], onError: Box[Throwable => Unit])(f: => T): Box[T] = {
    try {
      Full(f)
    } catch {
      case c if ignore.exists(_.isAssignableFrom(c.getClass)) => onError.foreach(_(c)); Empty
      case c if ignore == null || ignore.isEmpty => onError.foreach(_(c)); FailureBox(c.getMessage, Full(c), Empty)
    }
  }

  /**
   * Wraps a "try" block around the function f. If f throws
   * an exception that is in the domain of the handler PF,
   * the handler will be invoked on the exception. Otherwise
   * the exception is wrapped into a Failure.
   *
   * @param handler - A partial function that handles exceptions
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   </ul>
   * @see net.liftweb.common.Failure
   */
  def tryo[T](handler: PartialFunction[Throwable, T], f: => T): Box[T] = {
    try {
      Full(f)
    } catch {
      case t if handler.isDefinedAt(t) => Full(handler(t))
      case e => FailureBox(e.getMessage, Full(e), Empty)
    }
  }

  /**
   * Wraps a "try" block around the function f
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   </ul>
   */
  def tryo[T](f: => T): Box[T] = tryo(Nil, Empty)(f)


  /**
   * Wraps a "try" block around the function f and trigger a callback function if an exception is thrown
   * @param onError - an optional callback function that will use the thrown exception as a parameter
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   </ul>
   */
  def tryo[T](onError: Throwable => Unit)(f: => T): Box[T] = tryo(Nil, Full(onError))(f)

  /**
   * Wraps a "try" block around the function f
   * @param ignore - a list of exception classes to ignore. A thrown exception will be ignored if it is assignable from one of
   * the exception classes in the list
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: List[Class[_]])(f: => T): Box[T] = tryo(ignore, Empty)(f)

  /**
   * Wraps a "try" block around the function f. Takes only one Class of exception to ignore
   * @param ignore - a single exception classes to ignore. A thrown exception will be ignored if it is assignable from this class.
   * @param f - the block of code to evaluate
   * @return <ul>
   *   <li>Full(result of the evaluation of f) if f doesn't throw any exception
   *   <li>a Failure if f throws an exception
   *   <li>Empty if the exception class is in the ignore list
   *   </ul>
   */
  def tryo[T](ignore: Class[_])(f: => T): Box[T] = tryo(List(ignore), Empty)(f)

  
  
  
  /** short alias for java.security.SecureRandom */
  private val _random = {
    val r = new SecureRandom
    if (r == null) new Random else r
  }

  /**
   * Get a guaranteed unique field name
   * (16 or 17 letters and numbers, starting with a letter)
   */
  def nextFuncName: String = nextFuncName(0)

  /**
   * Get a guaranteed unique field name
   * (16 or 17 letters and numbers, starting with a letter)
   */
  def nextFuncName(seed: Long): String = {
    val sb = new StringBuilder(24)
    sb.append('F')
    sb.append(nextNum + seed)
    // sb.append('_')
    sb.append(randomString(6))
    sb.toString
  }
  
  /**
   * Create a random string of a given size.  5 bits of randomness per character
   * @param size size of the string to create. Must be a positive integer.
   * @return the generated string
   */
  def randomString(size: Int): String = {
    @scala.annotation.tailrec
    def addChar(pos: Int, lastRand: Int, sb: GoodSB): GoodSB = {
      if (pos >= size) sb
      else {
        val randNum = if ((pos % 6) == 0) {
          withRng(_.nextInt)
        } else {
          lastRand
        }

        sb.append(randNum & 0x1f match {
          case n if n < 26 => ('A' + n).toChar
          case n => ('0' + (n - 26)).toChar
        })
        addChar(pos + 1, randNum >> 5, sb)
      }
    }
    addChar(0, 0, new GoodSB(size)).toString
  }
  
  private lazy val _slowRandom = {
    val r = new SecureRandom
    if (r == null) new Random else r
  }
  private def withRng[T](block: Random =>T) = {
    // BUG 7051516 remove before JDK8 release
    /*
    _currentTlrMethod.map { meth =>
      block(meth.invoke(null).asInstanceOf[Random])
    } openOr {
      _slowRandom.synchronized(block(_slowRandom))
    }
    * */
    
    _slowRandom.synchronized(block(_slowRandom))
  }
  
  private val serial = new AtomicLong(math.abs(randomLong(System.currentTimeMillis)) + 1000000L)

  /**
   * Get a monotonically increasing number that's guaranteed to be unique for the
   * current session
   */
  def nextNum: Long = serial.incrementAndGet
  
  
  /**
   * Convert any object to an "equivalent" Boolean depending on its value
   */
  @scala.annotation.tailrec
  def toBoolean(in: Any): Boolean = {
    in match {
      case null => false
      case b : Boolean => b
      case i: Int => i != 0
      case lo: Long => lo != 0
      case n : Number => n.intValue != 0
      case s : String =>
        val sl = s.toLowerCase
        if (sl.length == 0) false
        else {
          if (sl.charAt(0) == 't') true
          else if (sl == "yes") true
          else toInt(s) != 0
        }
      case None => false
      case Empty | FailureBox(_, _, _) => false
      case Full(n) => toBoolean(n)
      case Some(n) => toBoolean(n)
      case x :: _ => toBoolean(x)
      case o => toBoolean(o.toString)
    }
  }
  
  /**
   * Convert any object to an "equivalent" Int depending on its value
   */
  @scala.annotation.tailrec
  def toInt(in: Any): Int = {
    in match {
      case null => 0
      case n: Int => n
      case lo: Long => lo.toInt
      case n : Number => n.intValue
      case (n: Number) :: _ => n.intValue
      case Some(n) => toInt(n)
      case Full(n) => toInt(n)
      case None | Empty | FailureBox(_, _, _) => 0
      case s: String => parseNumber(s).toInt
      case d: java.util.Date => (d.getTime / 1000L).toInt
      case x :: _ => toInt(x)
      case o => toInt(o.toString)
    }
  }
  
  /**
   * Convert any object to an "equivalent" Long depending on its value
   */
  @scala.annotation.tailrec
  def toLong(in: Any): Long = {
    in match {
      case null => 0L
      case i: Int => i
      case n: Long => n
      case d: java.util.Date => d.getTime
      case n : Number => n.longValue
      case (n: Number) :: _ => n.longValue
      case Some(n) => toLong(n)
      case Full(n) => toLong(n)
      case None | Empty | FailureBox(_, _, _) => 0L
      case s: String => parseNumber(s)
      case x :: _ => toLong(x)
      case o => toLong(o.toString)
    }
  }
  
  
  
  
  
  

  private def withRandom[T](f: Random => T): T = {
    _random.synchronized(f(_random))
  }

  /** return a random Long modulo a number */
  def randomLong(mod: Long): Long = withRandom(random => math.abs(random.nextLong) % mod)

  /** return a random int modulo a number */
  def randomInt(mod: Int): Int = withRandom(random => math.abs(random.nextInt) % mod)
}