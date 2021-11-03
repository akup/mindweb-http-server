package com.nn.mindweb.server.async

import akka.actor.ActorRef
import com.nn.http.{CometContext, CometListen, CometListener, CometUpdate, R, RenderVersion}
import com.nn.mindweb.server.dataadapter.AngularRequestServerStructure
import com.nn.mindweb.server.{Controller, ResponseBuilder, SessionMaster}
import org.pmw.tinylog.Logger
import net.aklabs.helpers.TimeHelpers._

import scala.concurrent.Promise
import scala.language.postfixOps

class CometAndAjaxController extends Controller {

  get("/mw_comet/:sess_id/:page_id")(req => {
    Logger.debug("Got comet: " + R.renderVersion + " : " + R.session)

    val theSession = R.session.get
    Logger.debug(theSession.uniqueId + " : " + req.routeParams("page_id"))

    val _c_actor: Option[ActorRef] = R.session.flatMap(sess => {
      sess.getAsyncComponent(Some(req.routeParams("page_id"))).headOption
    })

    Logger.debug("Got actor: " + _c_actor)

    if (SessionMaster.isDead(theSession.uniqueId) || !theSession.stateful_?) {
      render.plain("{\"sessionlost\": true}").status(200).header("Content-Type", "application/json;charset=UTF-8").toFuture
    } else if (_c_actor.isEmpty) {
      render.plain("{\"sessionrestore\": true}").status(200).header("Content-Type", "application/json;charset=UTF-8").toFuture
    } else {
      val c_actor = _c_actor.get

      val cancellable = CometContext.system.scheduler.scheduleOnce(2 minutes)({
        c_actor ! CometUpdate()
      })(CometContext.executionContext)
      val pr = Promise[ResponseBuilder]()

      val l = new CometListener() {
        def cometOut(json: String): Unit = {
          Logger.debug("Comet out")
          cancellable.cancel()
          pr.success(render.plain(json).status(200).header("Content-Type", "application/json;charset=UTF-8"))
        }
      }

      c_actor ! CometListen(l)

      pr.future
    }
  })
  get("/mw_ajax/:rendv_id")(req => {
    Logger.debug("Handle ajax")
    val renderVersion = req.routeParams("rendv_id")
    RenderVersion.doWith(renderVersion) {
      Logger.debug("Got ajax: " + R.renderVersion + " : " + R.session + " : " + req.session.map(_.linkedSuperSession))
      Logger.debug("Got ajax AngularRequestServerStructure: " + AngularRequestServerStructure.get)
      //val renderVersion = req.routeParams("rendv_id")
      if (req.params.get("heartbeat").isDefined) {
        R.session.foreach(_.updateFuncByOwner(renderVersion, System.currentTimeMillis()))
      } else {
        val runParamsResult: Option[Seq[Any]] = R.session.map(_.runParams(req))
        Logger.debug("runParamsResult: " + runParamsResult)
        /*
        R.renderVersion
        R.session
        */
        //state.paramNames
      }
    }

    render.plain("").status(200).header("Content-Type", "application/json;charset=UTF-8").toFuture
  })
}
