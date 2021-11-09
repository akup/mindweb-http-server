package net.aklabs.http.vars

import net.aklabs.helpers._
import net.aklabs.helpers.Helpers._
import org.pmw.tinylog.Logger

import scala.language.implicitConversions

// This object holds string constants in a central place
private[vars] object VarConstants {
  val varPrefix = "_v_"
  val initedSuffix = "_i_?"
  val lockSuffix="_l_"
}

abstract class AnyVar[T, MyType <: AnyVar[T, MyType]](dflt: => T) extends AnyVarTrait[T, MyType] {
  self: MyType =>

  protected def calcDefaultValue: T = dflt

  
}

/**
 * Abstract a request or a session scoped variable.
 */
trait AnyVarTrait[T, MyType <: AnyVarTrait[T, MyType]] extends PSettableValueHolder[T] {
  self: MyType =>
  
  protected lazy val name: String = "_mw_v_"+getClass.getName+"_"+_nameSalt
  protected def findFunc(name: String): Option[T]
  protected def setFunc(name: String, value: T): Unit
  protected def clearFunc(name: String): Unit

  private def _setFunc(name: String, value: T): Unit = {
    setFunc(name, value)

    val sd = settingDefault_?
    changeFuncs.foreach(f => tryo(f(Full(value), sd)))
  }

  private def _clearFunc(name: String): Unit = {
    Logger.debug("_clearFunc: " + name)
    clearFunc(name)
    changeFuncs.foreach(f => tryo(f(Empty, false)))
  }

  protected def wasInitialized(name: String): Boolean
  private var changeFuncs: List[FuncType] = Nil

  /**
   * The function takes a `Box[T]` (Full if the Var is being set, Empty if it's being cleared) and
   * a Boolean indicating that the set function is setting to the default value.
   *
   */
  type FuncType = (Box[T], Boolean) => Unit

  protected def calcDefaultValue: T


  /**
   * On any change to this Var, invoke the function. Changes are setting the value, clearing the value.
   * There may not be a call if the Var goes out of scope (e.g., a RequestVar at the end of the Request).
   *
   * The function takes a `Box[T]` (Full if the Var is being set, Empty if it's being cleared) and
   * a Boolean indicating that the set function is setting to the default value.
   *
   * The function should execute *very* quickly (e.g., Schedule a function to be executed on a different thread).
   *
   * The function should generally be set in Boot or when a singleton is created.
   *
   * @param f the function to execute on change
   */
  def onChange(f: FuncType): Unit = {
    changeFuncs ::= f
  }

  /**
   * A non-side-effecting test if the value was initialized
   */
  protected def testWasSet(name: String): Boolean

  protected def _nameSalt = ""

  /**
   * Keep track of whether we're currently setting the default value
   */
  private val settingDefault = new RichThreadLocal[Boolean]

  protected def settingDefault_? : Boolean = settingDefault.option getOrElse false

  type CleanUpParam

  /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism
   */
  def doSync[F](f: => F): F

  /**
   * The current value of the variable
   */
  def is: T = doSync {
    findFunc(name) match {
      case Some(v) => v
      case _ =>
        val ret = calcDefaultValue
        testInitialized
        settingDefault.doWith(true) {
          apply(ret)
        }
        // Use findFunc so that we clear the "unread" flag
        findFunc(name) match {
          case Some(v) => v
          case _ => ret
        }
    }
  }

  private def testInitialized(): Unit = doSync {
    if (!wasInitialized(name)) {
      registerCleanupFunc(_onShutdown _)
    }
  }

  /**
   * Shadow of the 'is' method
   */
  def get: T = is

  /**
   * Shadow of the apply method
   */
  def set(what: T): T = apply(what)

  /**
   * Has this Var been set or accessed and had its default value calculated
   */
  def set_? : Boolean = testWasSet(name)

  /**
   * Set the Var if it has not been calculated
   */
  def setIfUnset(value: => T): T = doSync {
    if (!set_?) {
      set(value)
    }
    this.is
  }

  /**
   * Set the session variable
   *
   * @param what -- the value to set the session variable to
   */
  def apply(what: T): T = {
    testInitialized()
    _setFunc(name, what)

    what
  }

  /**
   * Applies the given function to the contents of this
   * variable and sets the variable to the resulting value.
   *
   * @param f -- the function to apply and set the result from.
   */
  def update(f: T => T): T = {
    apply(f(is))
    is
  }

  def remove(): Unit = {
    _clearFunc(name)

  }

  //def cleanupFunc: Box[() => Unit] = Empty

  protected def registerCleanupFunc(in: CleanUpParam => Unit): Unit

  protected final def registerGlobalCleanupFunc(in: CleanUpParam => Unit): Unit = {
    cuf ::= in
  }

  private var cuf: List[CleanUpParam => Unit] = Nil

  private def _onShutdown(session: CleanUpParam): Unit = {
    cuf.foreach(f => tryo(f(session)))
    onShutdown(session)
  }

  protected def onShutdown(session: CleanUpParam): Unit = {}

  override def toString: String = is.toString

  /**
   * Change the value of the Var for the lifespan of the function
   */
  def doWith[F](newVal: T)(f: => F): F = {
    val old = findFunc(name)
    _setFunc(name, newVal)
    try {
      f
    } finally {
      old match {
        case Some(t) => _setFunc(name, t)
        case _ => _clearFunc(name)
      }
    }
  }
}

object AnyVar {
  implicit def whatVarIs[T](in: AnyVar[T, _]): T = in.is
}