package net.aklabs.helpers

/**
 * This is a decorator for a ThreadLocal variable that provides
 * convenience methods to transform the variable to a Box and execute
 * functions in a "scope" wherein the variable may hold a different value.
 */
class RichThreadLocal[T] {

  private val threadLocal = new ThreadLocal[T]

  /**
   * Returns the current value of this variable.
   */
  def value: T = threadLocal.get
  def is: T = threadLocal.get

  /**
   * Returns a Box containing the value of this ThreadGlobal
   * in a null-safe fashion.
   */
  def option: Option[T] = Option(value)
  def box: Box[T] = if (value == null) Empty else Full(value)

  /**
   * Sets the value of this ThreadGlobal.
   * @param v the value to set.
   */
  def set(v: T): RichThreadLocal[T] = {
    threadLocal.set(v)
    this
  }

  /**
   * Alias for <code>set(v: T)</code>
   * @param v the value to set.
   */
  def apply(v: T): RichThreadLocal[T] = set(v)

  /**
   * Sets this ThreadGlobal's contents to the specified value,
   * executes the specified function, and then restores the ThreadGlobal
   * to its earlier value. This effectively creates a scope within
   * the execution of the current thread for the execution of the specified
   * function.
   *
   * @param x the value to temporarily set in this ThreadGlobal
   * @param f the function to execute
   */
  def doWith[R](x: T)(f : => R) : R = {
    val original = value
    try {
      threadLocal.set(x)
      f
    } finally {
      threadLocal.set(original)
    }
  }
}

class ThreadVar[T](dflt: => T) {
  private val threadLocal = new ThreadLocal[T] {
    override protected def initialValue: T = dflt
  }

  /**
   * Returns the current value of this variable.
   */
  def value: T = threadLocal.get
  def is: T = threadLocal.get
  def get: T = threadLocal.get

  /**
   * Returns a Box containing the value of this ThreadGlobal
   * in a null-safe fashion.
   */
  def option: Option[T] = Option(value)
  def box: Box[T] = if (value == null) Empty else Full(value)

  /**
   * Sets the value of this ThreadGlobal.
   * @param v the value to set.
   */
  def set(v: T): ThreadVar[T] = {
    threadLocal.set(v)
    this
  }

  /**
   * Alias for <code>set(v: T)</code>
   * @param v the value to set.
   */
  def apply(v: T): ThreadVar[T] = set(v)


  def atomicUpdate(f: T => T): T =
    performAtomicOperation(set(f(get)).get)

  /**
   * Perform an atomic operation on the Settable. By default
   * synchronizes the instance, but it could use other mechanisms
   */
  def performAtomicOperation[T](f: => T): T = synchronized {
    f
  }

  /**
   * Sets this ThreadGlobal's contents to the specified value,
   * executes the specified function, and then restores the ThreadGlobal
   * to its earlier value. This effectively creates a scope within
   * the execution of the current thread for the execution of the specified
   * function.
   *
   * @param x the value to temporarily set in this ThreadGlobal
   * @param f the function to execute
   */
  def doWith[R](x: T)(f : => R) : R = {
    val original = value
    try {
      threadLocal.set(x)
      f
    } finally {
      threadLocal.set(original)
    }
  }
}