package com.nn.http
package vars

import java.util.concurrent.ConcurrentHashMap

import net.aklabs.helpers.{Helpers, RichThreadLocal}
import org.pmw.tinylog.Logger

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

/**
 * A typesafe container for data with a lifetime nominally equivalent to the
 * lifetime of HttpSession attributes.
 *
 * <code>
 * object MySnippetCompanion {
 *   object mySessionVar extends SessionVar[String]("hello")
 * }
 * </code>
 *
 * The standard pattern is to create a singleton object extending SessionVar instead
 * of creating an instance variable of a concrete SessionVar subclass. This is preferred
 * because SessionVar will use the name of its instantiating class for part of its state
 * maintenance mechanism.
 *
 * If you find it necessary to create a SessionVar subclass of which there may be more
 * than one instance, it is necessary to override the __nameSalt() method to return
 * a unique salt value for each instance to prevent name collisions.
 *
 * Note: SessionVars can be used within CometActors
 *
 * @param dflt - the default value to be returned if none was set prior to
 * requesting a value to be returned from the container
 */

abstract class SessionVar[T](dflt: => T) extends AnyVar[T, SessionVar[T]](dflt) {
  override protected def findFunc(name: String): Option[T] = R.session match {
    case Some(s) => s.get(name)
    case _ =>
      if (Rules.throwOnOutOfScopeVarAccess) {
        throw new IllegalAccessException("Access to SessionVar outside a request or comet actor scope")
      }

      if (showWarningWhenAccessedOutOfSessionScope_?)
        Logger.warn("Getting a SessionVar " + name + " outside session scope")

      None
  }

  /**
   * Stateless session enforcement is new to Lift, but there
   * are some legacy issues in WebKit and allowing for a SessionVar
   * to be "magic" (settable even in stateless sessions) seems to be
   * an efficient, yet somewhat hacky, way around the issue
   */
  //private[liftweb] def magicSessionVar_? = false

  override protected def setFunc(name: String, value: T): Unit = R.session match {
    // If we're in a stateless session, don't allow SessionVar setting
    case Some(s) =>
      if (!s.stateful_? && !settingDefault_?)
      		throw new IllegalAccessException("setting a SessionVar in a stateless session: " + getClass.getName)

      s.set(name, value)
    case _ =>
      if (Rules.throwOnOutOfScopeVarAccess) {
        throw new IllegalAccessException("Access to SessionVar outside a request or comet actor scope")
      }

      if (showWarningWhenAccessedOutOfSessionScope_?)
        Logger.warn("Setting a SessionVar " + name + " to " + value + " outside session scope") // added warning per issue 188
  }

  /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism
   */
  def doSync[F](f: => F): F = R.session match {
    case Some(s) =>
      // lock the session while the Var-specific lock object is found/created
      val lockName = name + VarConstants.lockSuffix
      val lockObj = s.synchronized {
        s.get[AnyRef](lockName) match {
          case Some(lock) => lock
          case _ => val lock = new AnyRef
          s.set(lockName, lock)
          lock
        }
      }

      // execute the query in the scope of the lock obj
      lockObj.synchronized {
        f
      }
    case _ => f
  }

  def showWarningWhenAccessedOutOfSessionScope_? = false

  override protected def clearFunc(name: String): Unit = R.session.foreach(_.unset(name))

  override protected def wasInitialized(name: String): Boolean = {
    val bn = name + VarConstants.initedSuffix
    val old: Boolean = R.session.flatMap(_.get(bn)) getOrElse false
    R.session.foreach(_.set(bn, true))
    old
  }

  override protected def testWasSet(name: String): Boolean = {
    val bn = name + VarConstants.initedSuffix
    R.session.flatMap(_.get(name)).isDefined || (R.session.flatMap(_.get(bn)) getOrElse false)
  }

  protected override def registerCleanupFunc(in: TheSession => Unit): Unit =
    R.session.foreach(_.addSessionCleanup(in))

  type CleanUpParam = TheSession
}



private[vars] trait HasLogUnreadVal {
  def logUnreadVal: Boolean
}

/**
 * Create case objects that implement this trait and use the case objects to denote
 * specific SnapshotGroups for RequestVars
 */
trait RequestVarSnapshotGroup

/**
 * This subclass of RequestVars that allow the specification of a RequestVarSnapshotGroup.
 * You can create a snapshot of all the members of this group in RequestVar.snapshot
 */
abstract class SnapshotRequestVar[T](val group: RequestVarSnapshotGroup, d: => T) extends RequestVar[T](d) {

  /**
   * The Snapshot group this requestvar is part of
   */
  override def snapshotGroup: Option[RequestVarSnapshotGroup] = Some(group)
}

/**
 * The companion object to RequestVars
 */
object RequestVar {
  /**
   * Given a RequestVarSnapshotGroup, generate a function that will snapshot all the RequestVars in
   * that group.  When the function is run, the RequestVars will be set to the value they held
   * when they were snapshotted
   */
  def snapshot(group: RequestVarSnapshotGroup): () => Unit = {
    // capture the restore functions
    val funcs = RequestVarHandler.instancesOfGroup(group).map(_.snapshot())

    // return a function that applies all the restore functions
    () => funcs.foreach(_.apply())
  }
}

/**
 * A typesafe container for data with a lifetime nominally equivalent to the
 * lifetime of a page rendered by an HTTP request.
 * RequestVars maintain their value throughout the duration of the current HTTP
 * request and any callbacks for servicing AJAX calls associated with the rendered page.
 * RequestVar instances have no value at the beginning of request servicing (excluding
 * AJAX callbacks) and their value is discarded at the end of request processing.
 * They are commonly used to share values across many snippets. Basic usage:
 *
 * <code>
 * object MySnippetCompanion {
 *   object myRequestVar extends RequestVar[String]("hello")
 * }
 * </code>
 *
 * The standard pattern is to create a singleton object extending RequestVar instead
 * of creating an instance variable of a concrete RequestVar subclass. This is preferred
 * because RequestVar will use the name of its instantiating class for part of its state
 * maintenance mechanism.
 *
 * If you find it necessary to create a RequestVar subclass of which there may be more
 * than one instance, it is necessary to override the __nameSalt() method to return
 * a unique salt value for each instance to prevent name collisions.
 *
 * @param dflt - the default value to be returned if none was set prior to
 * requesting a value to be returned from the container
 */
abstract class RequestVar[T](dflt: => T) extends AnyVar[T, RequestVar[T]](dflt) with HasLogUnreadVal {
  type CleanUpParam = Option[TheSession]

  /**
   * Is this RequestVar a member of a snapshot group?  If so, specify the group here
   */
  def snapshotGroup: Option[RequestVarSnapshotGroup] = None

  /**
   * Return a function that, when applied, will set the value of the RequestVar to its
   * current value
   */
  def snapshot(): () => Unit = {
    if (set_?) {
      val v = this.get
      () => this.set(v)
    } else {
      () => this.remove()
    }
  }

  override protected def findFunc(name: String): Option[T] = RequestVarHandler.get(name)

  override protected def setFunc(name: String, value: T): Unit = RequestVarHandler.set(name, this, value)

  override protected def clearFunc(name: String): Unit = RequestVarHandler.clear(name)

  override protected def wasInitialized(name: String): Boolean = {
    val bn = name + "_inited_?"
    val old: Boolean = RequestVarHandler.get(bn) getOrElse false
    RequestVarHandler.set(bn, this, true)
    old
  }

  /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism
   */
  def doSync[F](f: => F): F = f

  // no sync necessary for RequestVars... always on the same thread

  override protected def testWasSet(name: String): Boolean = {
    val bn = name + "_inited_?"
    RequestVarHandler.get(name).isDefined || (RequestVarHandler.get(bn) getOrElse false)
  }

  /**
   * Generate a function that will take a snapshot of the current RequestVars
   * such that they can be restored
   */
  final def generateSnapshotRestorer[T](): Function1[Function0[T], T] = RequestVarHandler.generateSnapshotRestorer()

  override protected def registerCleanupFunc(in: Option[TheSession] => Unit): Unit = {
    RequestVarHandler.addCleanupFunc(in)
  }

  /**
   * This defines whether or not Lift will log when a RequestVar is set but then not read within
   * the same request cycle. Change this to false to turn off logging. Logging can also be turned
   * off globally via LiftRules.logUnreadRequestVars.
   *
   * @see LiftRules#logUnreadRequestVars
   */
  def logUnreadVal = true
}

/**
 * A typesafe container for data with a lifetime strictly equal to the processing of a single
 * HTTP request. Unlike ordinary RequestVar instances, TransientRequestVars will not maintain
 * data for servicing of AJAX callbacks from a rendered page. This is useful in cases where
 * the value stored within the RequestVar cannot safely be used across multiple requests; an
 * example of such a value is a JTA UserTransaction which has a lifecycle strictly coupled
 * to the actul HTTP request handling by the enclosing container.
 *
 * @param dflt - the default value to be returned if none was set prior to
 * requesting a value to be returned from the container
 */
abstract class TransientRequestVar[T](dflt: => T) extends AnyVar[T, TransientRequestVar[T]](dflt) with HasLogUnreadVal {
  type CleanUpParam = Option[TheSession]

  override protected def findFunc(name: String): Option[T] = TransientRequestVarHandler.get(name)

  override protected def setFunc(name: String, value: T): Unit = TransientRequestVarHandler.set(name, this, value)

  override protected def clearFunc(name: String): Unit = TransientRequestVarHandler.clear(name)

  override protected def wasInitialized(name: String): Boolean = {
    val bn = name + "_inited_?"
    val old: Boolean = TransientRequestVarHandler.get(bn) getOrElse false
    TransientRequestVarHandler.set(bn, this, true)
    old
  }

  protected override def testWasSet(name: String): Boolean = {
    val bn = name + "_inited_?"
    TransientRequestVarHandler.get(name).isDefined || (TransientRequestVarHandler.get(bn) getOrElse false)
  }

  /**
   * Different Vars require different mechanisms for synchronization.  This method implements
   * the Var specific synchronization mechanism
   */
  def doSync[F](f: => F): F = f

  // no sync necessary for RequestVars... always on the same thread

  override protected def registerCleanupFunc(in: Option[TheSession] => Unit): Unit =
    TransientRequestVarHandler.addCleanupFunc(in)

  /**
   * This defines whether or not Lift will log when a RequestVar is set but then not read within
   * the same request cycle. Change this to false to turn off logging. Logging can also be turned
   * off globally via LiftRules.logUnreadRequestVars.
   *
   * @see LiftRules#logUnreadRequestVars
   */
  def logUnreadVal = false
}

trait CleanRequestVarOnSessionTransition {
  self: RequestVar[_] =>
}

object RequestVarHandler extends CoreRequestVarHandler {
  type MyType = RequestVar[_]

//TODO: was private[http], make more private
  def instancesOfGroup(grp: RequestVarSnapshotGroup): List[MyType] = {
    val cmp = Option(grp)
    for {
      bs <- backingStore.toList
      (rv, _, _) <- bs.values.asScala if rv.snapshotGroup == cmp
    } yield rv
  }
}

object TransientRequestVarHandler extends CoreRequestVarHandler {
  type MyType = TransientRequestVar[_]
}

trait CoreRequestVarHandler {
  type MyType <: HasLogUnreadVal

  //private val logger = Logger(classOf[CoreRequestVarHandler])
  // This maps from the RV name to (RV instance, value, set-but-not-read flag)
  private val vals: RichThreadLocal[ConcurrentHashMap[String, (MyType, Any, Boolean)]] = new RichThreadLocal()
  private val cleanup: RichThreadLocal[ListBuffer[Option[TheSession] => Unit]] = new RichThreadLocal
  private val isIn: RichThreadLocal[String] = new RichThreadLocal
  private val sessionThing: RichThreadLocal[Option[TheSession]] = new RichThreadLocal

  /**
   * Generate a function that will take a snapshot of the current RequestVars
   * such that they can be restored
   */
  final def generateSnapshotRestorer[T](): Function1[Function0[T], T] = {
    val myVals = vals.value
    val mySessionThing = sessionThing.value

    f => isIn.doWith("in")(
      vals.doWith(myVals)(
        cleanup.doWith(new ListBuffer) {
          sessionThing.doWith(mySessionThing) {
            //Logger.debug("run in snapshot context: " + vals.is.asScala.map(x => x._1 -> x._2._2))
            val ret: T = f()

            cleanup.value.toList.foreach(clean => Helpers.tryo(clean(sessionThing.value)))

            ret
          }
        }
      )
    )
  }

  protected def backingStore: Option[ConcurrentHashMap[String, (MyType, Any, Boolean)]] =
    vals.value match {
      case null =>
        if (System.getProperty("com.nn.finlift.throwOnOutOfScopeVarAccess", "false") == "true")
          throw new IllegalAccessException("Access to Var outside a request or comet actor scope")
        None
        /*
        if (LiftRules.throwOnOutOfScopeVarAccess) {
          throw new IllegalAccessException("Access to Var outside a request or comet actor scope")
        }
        None
        * 
        */
      case x => Some(x)
    }

  //TODO: was private[http], make more private
  def get[T](name: String): Option[T] =
    for {
      ht <- backingStore
      (rvInstance, value, unread) <- Option(ht.get(name))
    } yield {
      if (unread) {
        // Flag the variable as no longer being set-but-unread
        ht.put(name, (rvInstance: MyType, value.asInstanceOf[T], false))
      }
      value.asInstanceOf[T]
    }

  //TODO: was private[http], make more private
  private [http] def set[T](name: String, from: MyType, value: T): Unit =
    for (ht <- backingStore)
      ht.put(name, (from, value, true))

  //TODO: was private[http], make more private
  private [http] def clear(name: String): Unit =
    for (ht <- backingStore) {
      Logger.debug("clear from backing store: " + name)
      ht.remove(name)
    }

  //TODO: was private[http], make more private
  private [http] def addCleanupFunc(f: Option[TheSession] => Unit): Unit =
    for (cu <- Option(cleanup.value))
      cu += f

  def apply[T](session: Option[TheSession], f: => T): T = {
    if ("in" == isIn.value) {
      val tv = vals.value.asScala

      // remove all the session variables that are CleanRequestVarOnSessionTransition
      val toRemove: Iterable[String] = tv.flatMap {
        case (name, (_: CleanRequestVarOnSessionTransition, _, _)) => List(name)
        case _ => Nil
      }

      toRemove.foreach(n => tv -= n)


      sessionThing.set(session)
      f
    } else {
      isIn.doWith("in")(
        vals.doWith(new ConcurrentHashMap)(
          cleanup.doWith(new ListBuffer) {
            sessionThing.doWith(session) {
              val ret: T = f

              cleanup.value.toList.foreach(clean => Helpers.tryo(clean(sessionThing.value)))

              /*
              if (Props.devMode && LiftRules.logUnreadRequestVars) {
                vals.value.keys.filter(!_.startsWith(VarConstants.varPrefix + "net.liftweb"))
                  .filter(!_.endsWith(VarConstants.initedSuffix))
                  .foreach(key => vals.value(key) match {
                  case (rv, _, true) if rv.logUnreadVal => logger.warn("RequestVar %s was set but not read".format(key.replace(VarConstants.varPrefix, "")))
                  case _ =>
                })
              }
              * 
              */

              ret
            }
          }
        )
      )
    }
  }
}


object Vars {
  implicit def whatSessionVarIs[T](in: SessionVar[T]): T = in.is

  implicit def whatRequestVarIs[T](in: RequestVar[T]): T = in.is

  implicit def whatTransientRequestVarIs[T](in: TransientRequestVar[T]): T = in.is
}