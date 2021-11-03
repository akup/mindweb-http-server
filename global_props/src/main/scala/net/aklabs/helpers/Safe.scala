package net.aklabs.helpers

/**
 * Manage the current "safety" state of the stack
 */
object Safe {
  private val threadLocal = new RichThreadLocal[Int]

  /**
   * Is the current context "safe" for the object with the
   * given safety code?
   */
  def safe_?(test : Int) : Boolean = test == threadLocal.value

  /**
   * Marks access to a given object as safe for the duration of the function
   */
  def runSafe[T](x : Int)(f : => T) : T = {
    threadLocal.doWith(x)(f)
  }

  def randomString(len: Int): String = StringHelpers.randomString(len)
}

