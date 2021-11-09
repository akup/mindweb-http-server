package com.nn.mindweb.server
package ajaxsite

import java.io._
import java.util.concurrent.Executors
import com.aklabs.login.EdgeUser
import com.github.mustachejava.codes.DefaultMustache
import com.github.mustachejava.{Mustache, MustacheException, MustacheNotFoundException}
import com.twitter.mustache.ScalaObjectHandler
import net.aklabs.db.dbapi.{DBNodeRequest, DBOperationStatus}
import net.aklabs.helpers.Helpers
import net.aklabs.helpers.JsonHelpers._
import net.aklabs.modest.Modest
import net.aklabs.http.{HttpRequest, MapParamMap, R, RenderVersion, RoundTripInfo}
import net.aklabs.regbox.must.{LocViewFactory, MindwebMustacheFactoryTrait, MustacheLoc, View}
import org.apache.commons.io.IOUtils
import org.pmw.tinylog.Logger

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.{Failure, Success}
import LocViewFactory.default_language

import dataadapter._
import com.nn.mindweb.server.dbgrpc.DbClient

class DataNotFoundException(msg: String) extends Exception(msg)
class SitemapMFactoryByReq(sitemap: SiteMap) {
  def factory(request: Request): SitemapMustacheFactory = {
    val mustacheFactory = new SitemapMustacheFactory(sitemap, request)
    mustacheFactory.setObjectHandler(new ScalaObjectHandler)
    mustacheFactory.setExecutorService(Executors.newCachedThreadPool)

    mustacheFactory
  }
}
object SitemapMFactory {
  def factory(sitemap: SiteMap): SitemapMFactoryByReq = new SitemapMFactoryByReq(sitemap)
}
class SitemapMustacheFactory(sitemap: SiteMap, request: Request) extends MindwebMustacheFactoryTrait {
  protected lazy val sm = "<%"
  protected lazy val em = "%>"

  private def combinePaths(path1: String, path2: String): String = {
    new File(new File(path1), path2).getPath
  }

  private val partialCache: ThreadLocal[java.util.Map[String, Mustache]] = ThreadLocal.withInitial(() => new java.util.HashMap[String, Mustache])

  override def compile(reader: Reader, file: String, sm: String, em: String): Mustache = {
    val compile = mc.compile(reader, file, sm, em)
    compile.init()
    partialCache.remove()
    compile
  }

  override def compilePartial(s: String): Mustache = {
    val cache = partialCache.get
    val cached = cache.get(s)
    if (cached != null) { // Our implementation supports this but I
      // don't think it makes sense in the interface
      if (cached.isInstanceOf[DefaultMustache]) cached.asInstanceOf[DefaultMustache].setRecursive()
      return cached
    }
    try {
      val reader = getReader(s)
      if (reader == null) throw new MustacheNotFoundException(s)
      val mustache = mc.compile(reader, s, sm, em)
      //val mustache = mc.compile(s)
      cache.put(s, mustache)
      mustache.init()
      mustache
    } finally cache.remove(s)
  }

  override def getReader(_resourceName:String): Reader = {
    ///_resourceName.replaceAll()
    Logger.debug("Get inner resource: " + _resourceName)
    Logger.debug("basePath: " + System.getProperty("com.nn.regbox.docRoot"))
    var resourceName = _resourceName
    while (resourceName.indexOf("sitemap/sitemap/") > -1) {
      resourceName = _resourceName.replaceAll("sitemap/sitemap/", "sitemap/")
    }
    if (!resourceName.startsWith("sitemap/")) resourceName = "sitemap/%s".format(resourceName)
    if (resourceName.startsWith("sitemap/#")) {
      val sitemapLoadTo = resourceName.substring(8)

      val path = sitemap.findInnerPathByFullPath(request.path, sitemapLoadTo)
      Logger.debug("PATH: " + path)

      path.flatMap(sitemap.findFile(_)).flatMap(filePath => {
        Logger.debug("filePath: " + filePath)
        val fileFullPath = "%s/%s".format(sitemap.loadedFrom.get, filePath)
          .replaceAll("""\./""", "")

        //Logger.debug("fileFullPath: " + fileFullPath)
        //Logger.debug(SiteMap.loadedSitemap)

        SiteMap.rwl.readLock().lock()
        val res = SiteMap.loadedSitemap.get(fileFullPath)
        SiteMap.rwl.readLock().unlock()
        res
      }) match {
        case Some(content) =>
          new StringReader(content)
        case _ => throw new MustacheException("Mustache Template '" + sitemapLoadTo + "' should be found by path")
      }
    } else {
      val fileName = if (resourceName contains ".mustache") resourceName else resourceName+".mustache"
      val basePath = System.getProperty("com.nn.regbox.docRoot")//combinePaths(System.getProperty("com.nn.regbox.absRoot"), baseTemplatePath)
      val file = new File(basePath, fileName)
      if (file.exists() && file.isFile) {
        try {
          new BufferedReader(new InputStreamReader(new FileInputStream(file),"UTF-8"))
        } catch {
          case exception:FileNotFoundException =>
            throw new MustacheException("Found Mustache file, could not open: " + file + " at path: " + basePath, exception)
        }
      } else {
        throw new MustacheException("Mustache Template '" + resourceName + "' not found at " + file + " at path: " + basePath)
      }
    }
  }

  /**
   * Invalidate template caches during development
   */
  def invalidateMustacheCaches(): Unit = {
    mustacheCache.clear()
    templateCache.clear()
  }
}

class AjaxMapController(val rootPath: String, mustacheFactory: SitemapMFactoryByReq) extends Controller {
  def getTemplates(_path: (String, SitePathInfo), root: SiteMap,
                   parent: Option[SiteMap], loadTo: Option[String] = None,
                   hasTemplates: Seq[String] = Nil): (Option[String], Seq[(String, String)]) = { //template
    val parentLoadPath = parent.map(_.findLoadToParent(_path._1, loadTo, isAjax = true)).getOrElse(Nil)
    SiteMap.rwl.readLock().lock()
    val fullSitePathInfos = parentLoadPath :+ _path
    val loginRedirect = fullSitePathInfos.map(x => x._2 -> x._2.getLoginRedirect()).find(x => {
      x._2.nonEmpty
    }).flatMap(x => {
      x._1.getUserRedirect().orElse(x._2)
    })

    if (loginRedirect.nonEmpty) loginRedirect -> Nil
    else None -> {
      var nextPaths: Seq[(String, String, Map[String, String], Set[String])] =
        fullSitePathInfos.map(p => (p._1, p._2.filePath, p._2.targetDefaults, p._2.loadToTargets.keySet))
      var collectedPaths = nextPaths
      var additionals: Seq[(String, String, Map[String, String], Set[String])] = Nil
      var allTemplates: Seq[(String, String)] = Nil
      while (nextPaths.nonEmpty) {
        additionals = Nil
        allTemplates ++= nextPaths.flatMap(p => {
          if (!hasTemplates.contains(p._1)) {
            val filePath = "%s/%s".format(rootPath, p._2)
            SiteMap.loadedSitemap.get(filePath).flatMap(template => {
              """<%>sitemap/(.*?)%>""".r.findAllMatchIn(template).foreach(m => {
                p._3.get(m.group(1)).foreach(path => {
                  root.findInnerPathByFullPath(path, m.group(1)).foreach(x => {
                    if (!collectedPaths.exists(_._1 == x)) {
                      root.findSitePathInfo(x).foreach(sp => {
                        additionals :+= (x, sp.filePath, sp.targetDefaults, sp.loadToTargets.keySet)
                        collectedPaths :+= (x, sp.filePath, sp.targetDefaults, sp.loadToTargets.keySet)
                      })
                    }
                  })
                })
              })

              if (loadTo.exists(p._4.contains(_))) {
                allTemplates = Nil
                None
              } else
                Some(p._1 -> template)
            })
          } else None
        })

        nextPaths = additionals
      }
      SiteMap.rwl.readLock().unlock()

      Logger.debug("Has inner templates:")
      Logger.debug(SiteMap.loadedInnerTemplateNames)
      Logger.debug(SiteMap.loadedInnerTemplates)
      var collectedInner: Set[String] = Set()
      val innerTemplates = allTemplates.flatMap(t => {
        val templatePath = t._1
        SiteMap.loadedInnerTemplateNames.getOrElse(templatePath, Nil).flatMap(tName => {
          if (!hasTemplates.contains(tName) && !collectedInner.contains(tName)) {
            collectedInner += tName
            Some(tName -> SiteMap.loadedInnerTemplates(tName))
          } else None
        })
      })

      allTemplates ++ innerTemplates
    }
  }
  def defaultAjax(r: Request, _path: (String, SitePathInfo), root: SiteMap, parent: Option[SiteMap],
                  loadTo: Option[String] = None, isAjax: Boolean = false,
                  onlyData: Boolean = false, reqFromForm: Boolean = false,
                  hasTemplates: Seq[String] = Nil, hasLocalizations: Seq[String] = Nil,
                  language: Option[String] = None): Future[ResponseBuilder] = {
    Logger.debug("Default ajax load to: " + loadTo)
    val parentLoadPath = parent.map(_.findLoadToParent(_path._1, loadTo, isAjax)).getOrElse(Nil)
    val parentPath = parentLoadPath.headOption.getOrElse(_path)
    val filePath = "%s/%s".format(rootPath, parentPath._2.filePath)

    Logger.debug("parentLoadPath DONE")
    Logger.debug("parentLoadPath: " + parentLoadPath.map(_._1))

    val loginRedirect = (parentLoadPath :+ _path).map(x => x._2 -> x._2.getLoginRedirect()).find(x => {
      x._2.nonEmpty
    }).flatMap(x => {
      x._1.getUserRedirect().orElse(x._2)
    })

    import ServerContext.flow_dispatcher
    loginRedirect.map(redirect => {
      Logger.debug("Got redirect: " + redirect)
      Future{render.redirect(redirect)}
    }).getOrElse{
      //Logger.debug("Default ajax: " + parentLoadPath.map(x => x._1 -> (x._2.filePath, x._2.title, x._2.loadToTargets)))

      val lng = language.getOrElse(default_language)
      def sPath(path: String, pathInfo: SitePathInfo): (String, Seq[(String, String)], Option[String]) = {
        (path, pathInfo.loadToTargets.toSeq.flatMap(t => {
          Logger.debug(_path._1 + " : " + t._2.toSeq)
          val seq = t._2.toSeq.filter(targetPath => _path._1.startsWith(targetPath) &&
            _path._1.length >= targetPath.length)
            .map(tp => t._1 -> tp)
          if (seq.isEmpty) pathInfo.targetDefaults.get(t._1).map(t._1 -> _).toSeq else seq
        }).sortWith(_._2.length > _._2.length), pathInfo.title)
      }

      val sitemap_path = (parent.map(_.findLoadToParent(_path._1, loadTo)).getOrElse(Nil) :+ _path).map(p => {
        sPath(p._1, p._2)
      })

      @scala.annotation.tailrec
      def getChildTitle(sitemap_path: Option[(String, Seq[(String, String)], Option[String])]): Option[String] = {
        val childPath = sitemap_path.flatMap(_._2.headOption.map(_._2))
        val cp = childPath.flatMap(cp => {
          root.findSitePathInfo(cp).map(x => sPath(cp, x))
        })
        if (cp.isEmpty) sitemap_path.flatMap(_._3) else getChildTitle(cp)
      }

      val htmlTitle = getChildTitle(sitemap_path.lastOption).flatMap(
        MustacheLoc("sitemap", lng, Some("%s/".format(rootPath))).get(_).map(_.toString)).getOrElse("Без заголовка")

      /*
          Logger.debug("Title for: " + sitemap_path)
          val htmlTitle = sitemap_path.flatMap(_._3).lastOption
            .flatMap(x => {
              Logger.debug("Get title: " + rootPath + " : " + x)
              MustacheLoc("sitemap", lng, Some("%s/".format(rootPath))).get(x).map(_.toString)
            }).getOrElse("Без заголовка")
       */

      val sitemapJArr = JArray(sitemap_path.map(smp => JObject(JField("from", JString(smp._1)) :: {
          if (smp._2.isEmpty) Nil else
            JField("to", JArray(smp._2.map(p => JObject(JField("target", JString(p._1)) :: JField("path", JString(p._2)) :: Nil)))) :: Nil
        } ::: {if (smp._3.isEmpty) Nil else List(JField("title", JString(smp._3.get)))}
      )))


      val allDataRequests = (parentLoadPath.map(p => {
        (Request(SiteMap.restorePathName(p._1, r.routeParams)), p._2.dbRequest, p._1)
      }) ++ Seq(
        (Request(SiteMap.restorePathName(_path._1, r.routeParams)), _path._2.dbRequest, _path._1)
      ) ++ _path._2.targetDefaults.toSeq.map(p => {
        val path = p._2
        (Request(SiteMap.restorePathName(path, r.routeParams)), root.findSitePathInfo(path).flatMap(_.dbRequest), path)
      })).map(_req => {
        //Logger.debug("Request path: " + _req._1.path + " : " + _req._1.params)
        //Logger.debug("Request params: " + r.routeParams)
        val req = _req._1
        //r.routeParams.foreach(req.routeParams += _)
        req.mapParams = new MapParamMap(underlying = r.params.keySet.map(key => key -> r.params.getAll(key).toList).toMap)
        (req, _req._2, _req._3, r.routeParams)
      })

      val longestPath = allDataRequests.map(_._3).sortWith(_.compareTo(_) > 0).headOption
      /* :+ {
        r -> _path._2.dbRequest
      }*/

      Logger.debug("allDataRequests: " + allDataRequests)

      val renderVersion = R.renderVersion
      val session = R.session

      val dataRequests = allDataRequests.flatMap(r => {
        MindwebServer.fillDataRequestRouteParams(r._1)
        r._2.map(x =>
          DBNodeRequest(r._3, x._1, r._4.toSeq.flatMap(key_value => {
            val key = key_value._1
            Helpers.tryo {
              key.toInt
            }.map(_ -> key_value._2)
          }).sortWith(_._1 < _._1).map(_._2), Some(x._2))
        )
      })

      val dbDataFuture = if (dataRequests.isEmpty) Future{Nil} else {
        val dataRequestStartT = System.currentTimeMillis()
        DbClient.get(dataRequests, Some(isAjax)).map(data => {
          val status = data.status
          Logger.debug("Data response status: " + status)
          Logger.debug(data.data)
          if (status == DBOperationStatus.busy) {
            dataRequests.map(dr => {
              DataResponse(dr.path, data_js = Map("dbdata" -> Map("status" -> status)))
            })
          } else if (status == DBOperationStatus.empty) {
            Nil
          } else if (status == DBOperationStatus.notfound) {
            val dataEl = data.data.head
            throw new DataNotFoundException("Запись '%s' с uuid '%s' не найдена".format(dataEl.record, dataEl.error.getOrElse("error")))
          } else {
            val dataRequestEndT = System.currentTimeMillis()
            R.init(HttpRequest.nil(), session.get) {
              RenderVersion.doWith(renderVersion) {
                val dataResponses = data.data.map(dEl => {
                  val requestName = dataRequests.find(_.path == dEl.path).get.requestName
                  val operationIdPattern = data.opPatterns.find(_.requestName == requestName).map(_.operationIdPattern)
                  dataRequests.find(_.path == dEl.path).get.requestName
                  val d = dEl.error.map(e => Map("status" -> DBOperationStatus.error, "error" -> e)).orElse(
                    dEl.message.map(msg => {
                      Map("status" -> DBOperationStatus.ok,
                        dEl.record -> {
                          val (model, structure) = try {StructuredFormDataAdapter.modelAndStructureForPage(
                            Jckson.parse(msg),
                            withStruct = dEl.edit.getOrElse(false), Nil,
                            dEl.path, dEl.record, requestName, operationIdPattern,
                            data.vars.filter(_.path == dEl.path).map(
                              _.vars.map(v => v.key -> (Jckson.parse(v.value), (v.vckey.cacheType, v.vckey.key)))
                            ).foldLeft(Nil: Seq[(String, (JValue, (Int, String)))])(_ ++ _).toMap,
                            dEl.hiddens.map(h => h.path -> Jckson.parse(h.struct)).toMap
                          )} catch {
                            case e: Throwable =>
                              e.printStackTrace()
                              throw e
                          }
                          JObject(JField("structure", structure) ::
                            JField("model", model) ::
                            JField("js_validations", JArray(dEl.vFuncs.map(vf =>
                              JObject(JField(vf.path, JString(vf.struct)) :: Nil)
                            ))) ::
                            Nil)
                        }
                      )
                    })
                  ).getOrElse(Map("status" -> DBOperationStatus.error, "error" -> "Нет данных"))

                  DataResponse(dEl.path, data_js = Map("dbdata" -> d),
                    roundTrip = List[RoundTripInfo](
                      "save" -> StructuredFormDataAdapter.save _
                    ))
                })

                val dataRequestEndModifyT = System.currentTimeMillis()

                Logger.debug("Data request length: " + (dataRequestEndT - dataRequestStartT))
                Logger.debug("Data request + modify length: " + (dataRequestEndModifyT - dataRequestStartT))
                Logger.debug(dataRequestEndModifyT - dataRequestEndT)

                if (dataResponses.isEmpty) Nil else {
                  dataResponses ++ data.vars.map(v => {
                    DataResponse(v.path,
                      data_js = Map("dbvars" -> v.vars.map(_v => {
                        //val enum, dbenum, option, plink, loclink = Value
                        if (_v.vckey.cacheType == 2)
                          _v.key -> _v.value
                        else
                          _v.key.toInt -> _v.value
                      }).toMap)
                    )
                  })
                }
              }
            }
          }
        })
      }
      Future.sequence(allDataRequests.map(_req => {
        val req = _req._1
        R.init(req, session.get){
          RenderVersion.doWith(renderVersion){
            val attempted = MindwebServer.attemptDataRequest(req, reqFromForm)
            Logger.debug("attempted")
            Logger.debug(_req)
            Logger.debug(attempted)
            attempted
          }
        }
      })).map(_.flatten).flatMap(seq => {
        dbDataFuture.map(dseq => {
          var dataRequestMap = seq.map(dr => dr.path -> dr).groupBy(_._1).map(dr => {
            val template_js = dr._2.map(_._2.template_js).foldLeft(Map[String, Any]())(_ ++ _)
            val data_js = dr._2.map(_._2.data_js).foldLeft(Map[String, Any]())(_ ++ _)
            val roundTrip = dr._2.map(_._2.roundTrip).foldLeft(Seq[RoundTripInfo]())(_ ++ _)
            val language = dr._2.map(_._2.language).foldLeft(None: Option[String])(_.orElse(_))
            val loc_res = dr._2.map(_._2.loc_res).foldLeft(None: Option[String])(_.orElse(_))
            dr._1 -> DataResponse(dr._1,
              template_js,
              data_js,
              language,
              loc_res,
              roundTrip
            )
          })
          //Logger.debug("Collect: " + dataRequestMap)
          dseq.foreach(dr => {
            //Logger.debug("Collect dseq: " + dr.path)
            dr.path -> (dataRequestMap.get(dr.path) match {
              case Some(nonDbDataResponse) =>
                dataRequestMap += dr.path -> DataResponse(dr.path,
                  nonDbDataResponse.template_js,
                  dr.data_js ++ nonDbDataResponse.data_js,
                  nonDbDataResponse.language,
                  nonDbDataResponse.loc_res,
                  dr.roundTrip ++ nonDbDataResponse.roundTrip
                )
              case _ => dataRequestMap += dr.path -> dr
            })
          })
          //Logger.debug("After data merge: " + dataRequestMap)
          dataRequestMap.values.toList
        })
      })
        .flatMap(data => {
          val (view, template, templateData, jsDatas, roundtrips, localizations, locNames):
            (Option[View], Seq[(String, String)], Option[JValue], Seq[(String, JValue)], Seq[(String, String)], Seq[(String, JValue)], Seq[String]) =
            R.init(r, session.get){RenderVersion.doWith(renderVersion) {
              try {
                val rtInfos = data.map(d => d.path -> d.roundTrip).filter(_._2.nonEmpty)
                var lng = data.flatMap(_.language).headOption.getOrElse(default_language)
                //Logger.debug("Getting language: " + lng + " : " + data.flatMap(_.language))
                //Logger.debug("Getting default_language: " + default_language)

                var sitemapOut = false

                if (!isAjax) {
                  val locs = data.flatMap(_.loc_res).distinct.map(loc_res =>
                    loc_res -> MustacheLoc(loc_res, lng)
                  )
                  (Some(new View {
                    override def templateString: Option[String] = {
                      SiteMap.rwl.readLock().lock()
                      val ret = SiteMap.loadedSitemap.get(filePath)
                      SiteMap.rwl.readLock().unlock()
                      ret
                    }

                    override def templateFileName: Option[String] = None

                    override val factory: MindwebMustacheFactoryTrait = mustacheFactory.factory(r)

                    override protected lazy val sm = "<%"
                    override protected lazy val em = "%>"


                    val json: Any = LocViewFactory.localizeJson(data.flatMap(_.template_js.toSeq).toMap, lng)

                    //Logger.debug("data: " + data)
                    //Logger.debug("json: " + json)

                    val mw_page: String = renderVersion
                    val load_default_scripts: String = R.defaultScripts
                    val load_angular_platform: String = R.angularPlatformScripts
                    val enable_ajax: String = """<script>window.addEventListener('DOMContentLoaded', function() {%s})</script>""".format(R.ajaxScript())
                    val roundtrip_script: String = if (rtInfos.isEmpty) {
                      R.session.get.buildRoundtrip(Nil)
                      ""
                    } else {
                      """<script>window.addEventListener('DOMContentLoaded', function() {%s})</script>""".format(
                        rtInfos.map(i => {
                          """var _rtTemp_aj_nav = %s; window.insertRoundtrips(_rtTemp_aj_nav, '%s');""".format({
                            Logger.debug("Check render version at roundtrip")
                            Logger.debug(R.renderVersion)
                            Logger.debug(renderVersion)
                            R.session.get.buildRoundtrip(i._2)
                          }, i._1.replaceAll("'", "\'"))
                        }).mkString("\n")
                      )
                    }

                    val loc: Map[String, _] = locs.flatMap(_._2.toSeq).toMap
                    val locale: Map[String, Any] = Map(lng -> true, "lng" -> lng)
                    val locale_script: String = "<script>window.language = '" + lng + "';</script>"
                    //val path: String = _path._1

                    val ajax_nav_script_wrapper: String = "window.addEventListener('DOMContentLoaded', function() {"
                    val ajax_nav_script_wrapper_end: String = "})"
                    def sitemap_path(): String = {
                      if (sitemapOut) ""
                      else {
                        sitemapOut = true
                        "<script>window.addEventListener('DOMContentLoaded', function() {window.ajaxSitemapPath(%s, %s); window.loadedLocalizations(%s)})</script>".format(
                          sitemapJArr.toString,
                          longestPath.map("'%s'".format(_)).getOrElse("undefined"),
                          Jckson.serialize(Jckson.mapToJson(locs.toMap))
                        )
                      }
                    }
                    def json_data(): String = {
                      val jsonDatas = data.filter(!_.usedD).map(d => {d.usedD = true; d.path -> d.data_js})//.filter(_._2.nonEmpty)
                      Logger.debug("jsonDatas: " + jsonDatas)
                      if (jsonDatas.nonEmpty) {
                        "<script>window.addEventListener('DOMContentLoaded', function() {%s})</script>".format(
                          jsonDatas.map(d => "window.ajaxData('%s', %s);".format(
                            d._1.replaceAll("'", "\'"),
                            Jckson.serialize(Jckson.mapToJson(LocViewFactory.localizeJson(d._2, lng)))
                          )).mkString("\n")
                        )
                      } else ""
                    }
                    val title: String = htmlTitle
                  }), Nil, None, Nil, Nil, Nil, Nil)
                } else if (!onlyData) {
                  Logger.debug("View 2")
                  val locs = data.flatMap(_.loc_res).distinct.map(loc_res =>
                    loc_res -> MustacheLoc(loc_res, lng)
                  )
                  (Some(new View {
                    override def templateString: Option[String] = {
                      Logger.debug("templateString 2: " + hasTemplates)
                      if (!hasTemplates.contains(parentPath._2.filePath)) {
                        SiteMap.rwl.readLock().lock()
                        val ret = SiteMap.loadedSitemap.get(filePath)
                        SiteMap.rwl.readLock().unlock()
                        ret
                      } else None
                    }

                    override def templateFileName: Option[String] = None

                    override val factory: MindwebMustacheFactoryTrait = mustacheFactory.factory(r)

                    override protected lazy val sm = "<%"
                    override protected lazy val em = "%>"

                    val json: Any = LocViewFactory.localizeJson(data.flatMap(_.template_js.toSeq).toMap, lng)

                    Logger.debug("data: " + data)
                    Logger.debug("json: " + json)

                    val mw_page: String = renderVersion
                    val roundtrip_script: String = if (rtInfos.isEmpty) {
                      ""
                    } else {
                      """<script>%s</script>""".format(
                        rtInfos.map(i => {
                          """var _rtTemp_aj_nav = %s; window.insertRoundtrips(_rtTemp_aj_nav, '%s');""".format({
                            Logger.debug("Check render version at roundtrip")
                            Logger.debug(R.renderVersion)
                            Logger.debug(renderVersion)
                            R.session.get.buildRoundtrip(i._2)
                          }, i._1.replaceAll("'", "\'"))
                        }).mkString("\n")
                      )
                    }

                    val loc: Map[String, _] = locs.flatMap(_._2.toSeq).toMap
                    val locale: Map[String, Any] = Map(lng -> true, "lng" -> lng)
                    //val path: String = _path._1
                    //val locale_script: String = "<script>window.locale = '" + lng + "';</script>"

                    def sitemap_path(): String = {
                      if (sitemapOut) ""
                      else {
                        sitemapOut = true
                        """<script>window.ajaxSitemapPath(%s, %s); window.ajaxSitemapTitle('%s'); window.loadedLocalizations(%s);</script>"""
                          .format(
                            sitemapJArr.toString,
                            longestPath.map("'%s'".format(_)).getOrElse("undefined"),
                            htmlTitle.replaceAll("'", "\\'"),
                            Jckson.serialize(Jckson.mapToJson(locs.toMap))
                          )
                      }
                    }
                    def json_data(): String = {
                      val jsonDatas = data.filter(!_.usedD).map(d => {d.usedD = true; d.path -> d.data_js})//.filter(_._2.nonEmpty)
                      Logger.debug("jsonDatas: " + jsonDatas)
                      if (jsonDatas.nonEmpty) {
                        "<script>%s</script>".format(
                          jsonDatas.map(d => "window.ajaxData('%s', %s);".format(
                            d._1.replaceAll("'", "\'"),
                            Jckson.serialize(Jckson.mapToJson(LocViewFactory.localizeJson(d._2, lng)))
                          )).mkString("\n")
                        )
                      } else ""
                    }
                  }), Nil, None, Nil, Nil, Nil, Nil)
                } else {
                  val localizationNames = data.flatMap(_.loc_res).distinct
                  (None, { //template
                    SiteMap.rwl.readLock().lock()
                    var nextPaths: Seq[(String, String, Map[String, String], Set[String])] =
                      (parentLoadPath :+ _path).map(p => (p._1, p._2.filePath, p._2.targetDefaults, p._2.loadToTargets.keySet))

                    Logger.debug("Render templates nextPaths!")
                    Logger.debug(nextPaths)

                    var collectedPaths = nextPaths
                    var additionals: Seq[(String, String, Map[String, String], Set[String])] = Nil
                    var allTemplates: Seq[(String, String)] = Nil
                    while (nextPaths.nonEmpty) {
                      additionals = Nil
                      allTemplates ++= nextPaths.flatMap(p => {
                        if (!hasTemplates.contains(p._1)) {
                          val filePath = "%s/%s".format(rootPath, p._2)
                          SiteMap.loadedSitemap.get(filePath).flatMap(template => {
                            """<%>sitemap/(.*?)%>""".r.findAllMatchIn(template).foreach(m => {
                              p._3.get(m.group(1)).foreach(path => {
                                root.findInnerPathByFullPath(path, m.group(1)).foreach(x => {
                                  if (!collectedPaths.exists(_._1 == x)) {
                                    root.findSitePathInfo(x).foreach(sp => {
                                      additionals :+= (x, sp.filePath, sp.targetDefaults, sp.loadToTargets.keySet)
                                      collectedPaths :+= (x, sp.filePath, sp.targetDefaults, sp.loadToTargets.keySet)
                                    })
                                  }
                                })
                              })
                            })

                            if (loadTo.exists(p._4.contains(_))) {
                              allTemplates = Nil
                              None
                            } else
                              Some(p._1 -> template)
                          })
                        } else None
                      })

                      nextPaths = additionals
                    }
                    SiteMap.rwl.readLock().unlock()

                    val innerTemplates = allTemplates.flatMap(t => {
                      val templatePath = t._1
                      SiteMap.loadedInnerTemplateNames.getOrElse(templatePath, Nil).map(tName => {
                        tName -> SiteMap.loadedInnerTemplates(tName)
                      })
                    })

                    allTemplates ++ innerTemplates
                  }, Some(JObject(data.map(d => JField(d.path, Jckson.mapToJson( //template data with localizations
                    LocViewFactory.localizeJson(d.template_js, lng).asInstanceOf[Map[String, _]]
                  ))))), { //json data
                    data.map(d => d.path -> d.data_js)/*.filter(_._2.nonEmpty)*/.map(d => {
                      d._1 -> Jckson.mapToJson(LocViewFactory.localizeJson(d._2, lng))
                    })
                    //Jckson.mapToJson(LocViewFactory.localizeJson(data.flatMap(_.data_js.toSeq).toMap, lng))
                  }, rtInfos.map(i => {
                    Logger.debug("Check render version at roundtrip")
                    Logger.debug(R.renderVersion)
                    Logger.debug(renderVersion)
                    i._1 -> R.session.get.buildRoundtrip(i._2)
                  }), //roundtrips
                    localizationNames.filter(!hasLocalizations.contains(_)).flatMap(loc_res => { //localizations
                      val locMap = MustacheLoc(loc_res, lng)
                      if (locMap.nonEmpty) Some(loc_res -> Jckson.mapToJson(locMap)) else None
                    }),
                    localizationNames //all other localizations
                  )
                }
              } catch {
                case e: Throwable => e.printStackTrace(); throw e;
              }
            }}

          view.map(view => render.view(view)
            .header("Content-Type", "text/html; charset=utf-8").status(200).toFuture).getOrElse{

            render.json(
              JObject(
                JField("templates", JObject(template.map(t => JField(t._1, JString(t._2))))) ::
                  JField("locNames", JArray(locNames.map(JString(_)))) ::
                  JField("title", JString(htmlTitle)) ::
                  JField("sitemapPaths", sitemapJArr) ::
                  (if (roundtrips.nonEmpty)
                    JField("roundtrips", JObject(roundtrips.map(rt =>  JField(rt._1, JString(rt._2))))) :: Nil
                  else Nil) :::
                  (if (jsDatas.nonEmpty)
                    JField("data", JObject(jsDatas.map(rt =>  JField(rt._1, rt._2)))) :: Nil
                  else Nil) :::
                  (if (localizations.nonEmpty)
                    JField("locs", JObject(localizations.map(l =>  JField(l._1, l._2)))) :: Nil
                  else Nil) :::
                  templateData.map(JField("tdata", _) :: Nil).getOrElse(Nil)
              )
            ).status(200).toFuture
          }
        }).transform {
        case Success(result) =>
          Success(result)
        case Failure(exception) =>
          exception match {
            case e: DataNotFoundException =>
              e.printStackTrace()
              Success(render.notFound(Some(e.getMessage)))
            case e: Throwable =>
              e.printStackTrace()
              Success(render.internalError(Some(e.getMessage)))
          }
      }
    }
  }
  def lazyLoad(path: (String, SitePathInfo),
               uuid: String, thePath: String, reqNum: Int, descriptor: Int,
               renderVersion: String): Future[ResponseBuilder] = {
    import ServerContext.flow_dispatcher
    path._2.dbRequest.map(dbR => {
      val session = R.session
      val str_vars = RenderVersion.doWith(renderVersion) {
        AngularRequestServerStructure.getVars(path._1)
      }
      str_vars.map(str_vars => {
        DbClient.lazyLoad(dbR._1, uuid, reqNum, descriptor, str_vars).map(ll => {
          val parsedVars = ll.vars.map(v => {
            (v.key, Jckson.parse(v.value), (v.vckey.cacheType, v.vckey.key))
          })

          val data = R.init(HttpRequest.nil(), session.get) {RenderVersion.doWith(renderVersion) {
            AngularRequestServerStructure.addVars(
              path._1,
              parsedVars
            )
            Jckson.parse(ll.data) match {
              case obj: JObject =>
                obj \ "o" match {
                  case JArray(arr) =>
                    val transformed = arr.map(v => {
                      StructuredFormDataAdapter.transformToWebModel(v, Nil, Some(path._1))
                    }).toList
                    val objs = JArray(transformed.map(_._1))
                    StructuredFormDataAdapter.addToModelByPath(transformed.map(_._2), path._1, thePath, descriptor -> reqNum)
                    obj.set("o", objs)
                  case _ =>
                    obj
                }
              case _ =>
                JObject(JField("error", JString("Не известная ошибка")))
            }
          }}
          render.json(
            JObject(List(JField("data", data),
              JField("vars", JArray(parsedVars.map(v => {
                JObject(List(JField("k", {
                  if (v._3._1 == 2)
                    JString(v._1)
                  else
                    JInt(v._1.toInt)
                }), JField("v", v._2)))
              })))))
          )
        })
      }).getOrElse(Future{render.notFound(Some("No server structure for path " + path._1))})
    }).getOrElse(Future{render.notFound(Some("No db request for path " + path._1))})
  }
  def hiddenPick(path: (String, SitePathInfo), reqNum: Int, descriptor: Int, thePath: String,
                 renderVersion: String,
                 search: Option[String], limit: Option[Int]): Future[ResponseBuilder] = {
    import ServerContext.flow_dispatcher
    path._2.dbRequest.map(dbR => {
      val session = R.session
      val str_vars = RenderVersion.doWith(renderVersion) {
        AngularRequestServerStructure.getVars(path._1)
      }
      str_vars.map(str_vars => {
        DbClient.hiddenPick(dbR._1, search, limit, reqNum, descriptor, str_vars).map(ll => {
          val parsedVars = ll.vars.map(v => {
            (v.key, Jckson.parse(v.value), (v.vckey.cacheType, v.vckey.key))
          })
          val data = R.init(HttpRequest.nil(), session.get) {RenderVersion.doWith(renderVersion) {
            AngularRequestServerStructure.addVars(
              path._1,
              parsedVars
            )
            Jckson.parse(ll.data) match {
              case obj: JObject =>
                obj \ "o" match {
                  case JArray(arr) =>
                    val transformed = arr.map(v => {
                      StructuredFormDataAdapter.transformToWebModel(v, Nil, None)
                    }).toList
                    val objs = JArray(transformed.map(_._1))
                    obj.set("o", objs)
                  case _ =>
                    obj
                }
              case _ =>
                JObject(JField("error", JString("Не известная ошибка")))
            }
          }}
          render.json(
            JObject(List(JField("data", data),
              JField("vars", JArray(parsedVars.map(v => {
                JObject(List(JField("k", {
                  if (v._3._1 == 2)
                    JString(v._1)
                  else
                    JInt(v._1.toInt)
                }), JField("v", v._2)))
              })))))
          )
        })
      }).getOrElse(Future{render.notFound(Some("No server structure for path " + path._1))})
    }).getOrElse(Future{render.notFound(Some("No db request for path " + path._1))})
  }
}

case class SitePathInfo(filePath: String,
                        inner: Option[SiteMap] = None,
                        title: Option[String],
                        dbRequest: Option[(String, Boolean)],
                        nologinredirect: Option[String],
                        loginredirect: Option[String],
                        checkloginf: Option[String],
                        var hasAngularPlatform: Boolean) {
  var loadToTargets: Map[String, mutable.Set[String]] = Map.empty
  var targetDefaults: Map[String, String] = Map.empty
  def addTargetSelector(selector: String, loadPath: String): Boolean = {
    Logger.debug("addTargetSelector: " + selector)
    loadToTargets.getOrElse(selector, {
      val seq = mutable.Set[String]()
      loadToTargets += selector -> seq
      seq
    }).add(loadPath)
  }

  def serialize(): String = {
    "filePath: %s (loadToTargets: %s) (targetDefaults: %s), inner: %s".format(filePath,
      loadToTargets.toString(), targetDefaults.toString(),
      inner.map(_.serialize()).toString)
  }

  override def toString: String = {
    throw new Exception("toString")
    super.toString
  }


  def getLoginRedirect(): Option[String] = {
    var needToBeLoggedIn: Option[Boolean] = None
    val redirect = nologinredirect match {
      case Some(redirect) =>
        needToBeLoggedIn = Some(true)
        redirect
      case _ => loginredirect match {
        case Some(redirect) =>
          needToBeLoggedIn = Some(false)
          redirect
        case _ =>
          ""
      }
    }

    Logger.debug("getLoginRedirect: " + filePath + " : " + needToBeLoggedIn + " : " + EdgeUser.currentUser.isEmpty)

    if (needToBeLoggedIn.contains(EdgeUser.currentUser.isEmpty)) {
      Some(redirect)
    } else None
  }
  def getUserRedirect(): Option[String] = {
    EdgeUser.currentUser.flatMap(u => {
      None
    })
  }
}
object SiteMap {
  private[ajaxsite] val rwl = new java.util.concurrent.locks.ReentrantReadWriteLock()

  private[ajaxsite] var sitemapFilesModTime: Map[String, Long] = Map.empty
  private[ajaxsite] var loadedSitemap: Map[String, String] = Map.empty

  private[ajaxsite] var loadedInnerTemplates: Map[String, String] = Map.empty
  private[ajaxsite] var loadedInnerTemplateNames: Map[String, Seq[String]] = Map.empty

  private val starRegexp = """\*""".r
  private[ajaxsite] def restorePathName(p: String, routeParams: mutable.Map[String, String]) = {
    var i = 0
    starRegexp.replaceAllIn(p, _ => {
      i += 1
      routeParams(i.toString)
    })
  }

  private val innerTemplRegex = """<%>(.*?)%>""".r
  def cleanInnerTemplates(): Unit = {
    loadedInnerTemplates = Map.empty
    loadedInnerTemplateNames = Map.empty
  }
  def collectInnerTemplatesFor(templStr: String, fileName: String): Seq[(String, String)] = {
    Logger.debug("collectInnerTemplatesFor: " + fileName)
    loadedInnerTemplateNames.get(fileName).map(templs => {
      templs.map(tName => {
        tName -> loadedInnerTemplates(tName)
      })
    }).getOrElse{
      var collected: Seq[(String, String)] = Seq()
      innerTemplRegex.findAllMatchIn(templStr).foreach(m => {
        val templateName = m.group(1)
        if (!templateName.startsWith("sitemap/#")) {
          val fileNameInner = if (templateName contains ".mustache") templateName else templateName+".mustache"
          val basePath = System.getProperty("com.nn.regbox.sitemapRoot")//combinePaths(System.getProperty("com.nn.regbox.absRoot"), baseTemplatePath)
          val file = new File(basePath, fileNameInner)
          if (file.exists() && file.isFile) {
            Logger.debug("Adding inner")
            val templStrInner = IOUtils.toString(new FileInputStream(file), "UTF-8")
            val templateNameInner = fileNameInner.replaceAll("""\.mustache$""", "")
            collected :+= templateNameInner -> templStrInner
            collected ++= collectInnerTemplatesFor(templStrInner, templateNameInner)
          }
        }
      })
      loadedInnerTemplateNames += fileName -> collected.map(c => {
        if (!loadedInnerTemplates.contains(c._1))
          loadedInnerTemplates += c._1 -> c._2
        c._1
      })

      collected
    }
  }
  def fromJson(loadFromFolder: String, parent: Option[SiteMap] = None): SiteMap = {
    if (loadFromFolder == null)
      return SiteMap(Map.empty, loadedFrom = Some(null))
    var absoluteRootPath = new File(loadFromFolder).getAbsolutePath
    if (absoluteRootPath.last == '/') {
      absoluteRootPath = absoluteRootPath.substring(0, absoluteRootPath.length - 1)
    }
    absoluteRootPath = absoluteRootPath.replaceAll("""/\./""", "/")

    val sitemapYml = new File("%s/sitemap.yml".format(absoluteRootPath))
    if (sitemapYml.exists()) {
      val json = Jckson.parseYaml(sitemapYml).asInstanceOf[JArray]
      sitemapFilesModTime += sitemapYml.getAbsolutePath -> sitemapYml.lastModified()
      fromJson(json, parent, Some(absoluteRootPath))
    } else {
      SiteMap(Map.empty, loadedFrom = Some(absoluteRootPath))
    }
  }
  def fromJson(json: JArray, parent: Option[SiteMap], loadFrom: Option[String]): SiteMap = {
    val parentMap = SiteMap(Map.empty, parent, loadFrom)
    val paths = json.arr.flatMap(el => {
      var inner: Option[JArray] = None
      var title: Option[String] = None
      var dbrequest: Option[String] = None
      var dbrequestEdit: Boolean = false
      var nologinredirect: Option[String] = None
      var loginredirect: Option[String] = None
      var checkloginf: Option[String] = None
      el.children.flatMap(ch => {
        if (ch.name == "inner") {
          ch.value match {
            case a: JArray => inner = Some(a)
            case _ =>
          }
          None
        } else if (ch.name == "title") {
          ch.value match {
            case JString(s) => title = Some(s)
            case _ =>
          }
          None
        } else if (ch.name == "dbrequest") {
          ch.value match {
            case JString(s) => dbrequest = Some(s)
            case _ =>
          }
          None
        } else if (ch.name == "dbedit") {
          ch.value match {
            case JBool(b) => dbrequestEdit = b
            case _ =>
          }
          None
        } else if (ch.name == "nologinredirect") {
          ch.value match {
            case JString(s) => nologinredirect = Some(s)
            case _ =>
          }
          None
        } else if (ch.name == "loginredirect") {
          ch.value match {
            case JString(s) => loginredirect = Some(s)
            case _ =>
          }
          None
        } else if (ch.name == "checkloginf") {
          ch.value match {
            case JString(s) => checkloginf = Some(s)
            case _ =>
          }
          None
        } else ch.value match {
          case JString(s) => Some(ch.name -> s)
          case _ => None
        }
      }).toList.headOption.map(p => {
        p._1 -> SitePathInfo(p._2, inner.map(SiteMap.fromJson(_, Some(parentMap), None)),
          title,
          dbrequest.map(_ -> dbrequestEdit),
          nologinredirect, loginredirect, checkloginf,
          hasAngularPlatform = false)
      })
    }).toMap
    parentMap.paths = paths

    if (loadFrom.nonEmpty) {
      Logger.debug("Should start watch")
      parentMap.startWatch()
    }
    parentMap
  }
}
case class SiteMap(var paths: Map[String, SitePathInfo], parent: Option[SiteMap] = None,
                   loadedFrom: Option[String] = None) {
  private var parsed = false

  def root(): SiteMap = {
    parent.map(_.root()).getOrElse(this)
  }
  def pathToRoot(path: List[SiteMap] = Nil): List[SiteMap] = {
    parent.map(_.pathToRoot(this :: path)).getOrElse(this :: path)
  }
  def findLoadToParent(currentPath: String,
                       loadTo: Option[String] = None, isAjax: Boolean = false): Seq[(String, SitePathInfo)] = {
    Logger.debug("findLoadToParent " + currentPath + " : " + loadTo)
    Logger.debug("paths: " + paths.map(p => p._1 -> p._2.loadToTargets))
    val pI = paths.find(p => {
      if (currentPath.startsWith(p._1)) {
        p._2.loadToTargets.exists(_._2.exists(x => currentPath.startsWith(x)))
      } else false
    })
    Logger.debug("pI: " + pI.map(p => p._1 -> p._2.loadToTargets))

    if (isAjax && pI.nonEmpty) {
      if (loadTo.forall(lt => pI.get._2.loadToTargets.contains(lt)))
        Nil
      else
        parent.map(_.findLoadToParent(currentPath, loadTo, isAjax)).getOrElse(Nil) ++ pI.toSeq
    } else
      parent.map(_.findLoadToParent(currentPath)).getOrElse(Nil) ++ pI.toSeq
  }
  def pathStartsWithPath(checkPath: String, currentPath: String): Boolean = {
    val _checkPath = checkPath.split("/")
    val _currentPath = currentPath.split("/")

    //Logger.debug("pathStartsWithPath: " + _checkPath.toList)
    //Logger.debug(_currentPath.toList)

    var i = 0
    if (_currentPath.size <= _checkPath.size)
      _currentPath.forall(cp => {
        val res = _checkPath(i) == "*" || _checkPath(i) == cp
        i += 1
        res
      })
    else
      _checkPath.forall(chP => {
        val res = chP == "*" || _currentPath(i) == chP
        i += 1
        res
      })
  }
  def findInnerPathByFullPath(path: String, loadToTarget: String): Option[String] = {
    Logger.debug("findInnerPathByFullPath " + path + " : " + loadToTarget)
    paths.find(_._2.loadToTargets.contains(loadToTarget)).flatMap(p => {
      Logger.debug("got load to targets: " + p._1 + " : " + p._2.loadToTargets + " : " + loadToTarget)
      val paths = p._2.loadToTargets(loadToTarget).toSeq.filter(x => pathStartsWithPath(x, path))
      Logger.debug("find inner paths: " + paths)
      val pL = path.split("/").length
      paths.map(p => p -> p.split("/").length).filter(x => {x._2 <= pL})
        .sortWith(_._1.length > _._1.length).headOption.map(_._1).orElse{
        if (paths.nonEmpty) p._2.targetDefaults.get(loadToTarget) else None
      }
    }).orElse{
      paths.flatMap(_._2.inner.flatMap(_.findInnerPathByFullPath(path, loadToTarget))).headOption
    }
  }
  def findPath(filePath: String): Option[String] = {
    paths.find(_._2.filePath == filePath).map(_._1).orElse(paths.flatMap(p => {
      p._2.inner.flatMap(_.findPath(filePath))
    }).headOption)
  }
  def findSitePathInfo(path: String): Option[SitePathInfo] = {
    paths.get(path).orElse(paths.flatMap(p => {
      p._2.inner.flatMap(_.findSitePathInfo(path))
    }).headOption)
  }
  def findFile(path: String): Option[String] = {
    Logger.debug("findFile")
    Logger.debug(paths.keys)
    Logger.debug(path)
    Logger.debug("findFile done: " + paths.get(path).map(_.filePath))
    paths.get(path).map(_.filePath).orElse(paths.flatMap(p => {
      p._2.inner.flatMap(_.findFile(path))
    }).headOption)
  }
  def serialize(): String = {
    paths.map(p => p._1 -> p._2.serialize()).toString
  }

  def parseMap(): Unit = {
    if (!parsed && root().loadedFrom != null) {
      paths.foreach(p => {
        Logger.debug("Next parse: " + p._1 + " : " + p._2.filePath)
        val pathInfo = p._2
        val rootPath = root().loadedFrom.map("%s/".format(_)).getOrElse("./resources/sitemap/")
        val f = new File(rootPath, pathInfo.filePath)
        val fPath = f.getAbsolutePath.replaceAll("""\./""", "")
        if (f.exists()) {
          val htmlTree = Modest.parseFile(fPath)
          val htmlNodesDefaultLoadTo = htmlTree.select("[_ajD]")

          Logger.debug("Check mw platform")
          htmlTree.select("[mw-platform]").foreach(node => {
            Logger.debug("mw platform node: " + node.tagName + " : " + node.attributes.map(a => a.key -> a.value).toMap)
          })
          val hasAngularPlatform = htmlTree.select("[mw-platform]").length > 0

          var hasDefaultLoadPath = false
          //Logger.debug("htmlNodesDefaultLoadTo: " + htmlNodesDefaultLoadTo.length)
          try {
            htmlNodesDefaultLoadTo.foreach(node => {
              val attrMap = node.attributes.map(a => a.key -> a.value).toMap
              attrMap.get("_ajd") match {
                case Some(attrValue) =>
                  attrMap.get("id") match {
                    case Some(id) =>
                      root().findPath(attrValue) match {
                        case Some(thePath) =>
                          val idSelector = "#%s".format(id)
                          node.prependChildHtml("<%%>sitemap/%s%%>".format(idSelector))
                          p._2.addTargetSelector(idSelector, thePath)
                          p._2.targetDefaults += idSelector -> thePath
                        case _ =>
                          throw new Exception("Не можем найти путь в sitemap для шаблона '%s'".format(attrValue))
                      }

                    case _ =>
                      throw new Exception("Тег с параметром _ajD должен иметь id")
                  }
                  hasDefaultLoadPath = true

                case _ =>
                  throw new Exception("Параметр _ajD не должен быть пустым")
              }
            })

            val htmlNodesT = htmlTree.select("[_ajT]")
            htmlNodesT.foreach(node => {
              //Logger.debug(node.tagName)
              //Logger.debug(node.toString())
              val attrMap = node.attributes.map(a => a.key -> a.value).toMap
              val ajL = attrMap.get("_ajl") match {
                case Some(ajL) =>
                  if (ajL.isEmpty) {
                    if (node.tagName == "a")
                      attrMap.get("href")
                    else None
                  } else Some(ajL)
                case _ => None
              }

              ajL.foreach(ajL => {
                val selector = attrMap("_ajt")
                val htmlNodesDefaultLoadTo = htmlTree.select(selector)

                root().findFile(ajL) match {
                  case Some(_) =>
                    val foundSelector = if (htmlNodesDefaultLoadTo.nonEmpty) {
                      p._2.addTargetSelector(selector, ajL)
                      true
                    } else if (parent.nonEmpty) {
                      Logger.debug("HAS PARENT")
                      parent.get.addTargetSelector(selector, ajL)
                    } else false

                    Logger.debug("foundSelector: " + foundSelector)

                    if (!foundSelector)
                      throw new Exception("в атрибуте _ajT в файле " + f.getName + " содержится селектор " + selector + ", не имеющий соответствующего нода в файлах")
                  case _ =>
                    throw new Exception("Нет пути в sitemap '%s'".format(ajL))
                }
              })
            })

            val htmlBodyNodes = htmlTree.select("body")
            var treeString = if (htmlBodyNodes.nonEmpty) {
              val bodyNode = htmlBodyNodes.head

              val treeBefore = htmlTree.toString()

              if (treeBefore.indexOf("<%&locale_script%>") < 0)
                bodyNode.prependChildHtml("<%{locale_script}%>")
              if (treeBefore.indexOf("<%&sitemap_path%>") < 0)
                bodyNode.prependChildHtml("<%&sitemap_path%>")
              if (treeBefore.indexOf("<%&enable_ajax%>") < 0)
                bodyNode.prependChildHtml("<%&enable_ajax%>")
              if (treeBefore.indexOf("<%&roundtrip_script%>") < 0)
                bodyNode.prependChildHtml("<%&roundtrip_script%>")

              val _preTree = htmlTree.toString()
                .replaceAll("""<%current_path%>""", p._1.replaceAll("'", "\'"))
              if (treeBefore.indexOf("<%&json_data%>") < 0)
                _preTree.replaceFirst("</body>", "<%&json_data%></body>")
              else _preTree
            } else {
              "%s\n%s\n%s".format(
                htmlTree.toString()
                  .replaceAll("""<%current_path%>""", p._1.replaceAll("'", "\'")),
                "<%&sitemap_path%>", "<%&json_data%>")
            }

            SiteMap.collectInnerTemplatesFor(treeString, p._1)

            val isInAngularPlatform = if (htmlBodyNodes.isEmpty)
              pathToRoot().exists(sp => {
                sp.paths.toSeq.filter(_p => p._1.startsWith(_p._1))
                  .sortWith(_._1.length > _._1.length).headOption.exists(p => {
                  p._2.hasAngularPlatform
                })
              })
            else false
            pathInfo.hasAngularPlatform = hasAngularPlatform

            Logger.debug("Check is angular platfrom")
            Logger.debug(fPath + " -> " + isInAngularPlatform + " : " + pathToRoot().map(_.paths.keys))
            Logger.debug("hasAngularPlatform: " + hasAngularPlatform)

            if (isInAngularPlatform) {
              val closeTag = treeString.indexOf(">")
              val sb = new StringBuilder()
              sb.append(treeString.substring(0, closeTag))
              sb.append(" *mw-data=\"let model; let errors = errors; upd: upd;\" ")
              sb.append("path=\"")
              sb.append(p._1)
              sb.append("\"")
              sb.append(treeString.substring(closeTag))
              treeString = sb.toString()
            }

            SiteMap.loadedSitemap += fPath -> treeString
            SiteMap.sitemapFilesModTime += fPath -> f.lastModified()

            pathInfo.inner.foreach(_.parseMap())
          } finally {
            Logger.debug("Finally clean tree: " + p._1 + " : " + p._2.filePath)
            try {
              htmlTree.finalize()
            } catch {
              case e: Exception =>
            }
            Logger.debug("clean ok: " + p._1)
          }
        } else {
          SiteMap.loadedSitemap += fPath -> "no file"
          SiteMap.sitemapFilesModTime += fPath -> 0
        }
      })
    }
  }

  private[ajaxsite] def addTargetSelector(selector: String, loadPath: String): Boolean = {
    val exists = paths.exists(x => {
      val hasId = x._2.loadToTargets.get(selector)
      if (hasId.nonEmpty)
        x._2.addTargetSelector(selector, loadPath)
      hasId.nonEmpty
    })
    if (!exists) parent.exists(_.addTargetSelector(selector, loadPath))
    else true
  }

  private def addPaths(ctrl: AjaxMapController): Unit = {
    paths.foreach(path => {
      var num = 1
      val sinatraPath = "/%s".format(path._1.split("/").map(p => {
        val path = p.trim
        if (path == "*") {
          val extractPath = ":%d".format(num)
          num += 1
          extractPath
        } else path
      }).filter(_.nonEmpty).mkString("/"))
      Logger.debug(sinatraPath)
      ctrl.get(sinatraPath)(r => {
        //{lazyload: true, descriptor: structure.lazy, mwpage_id: mw_page},
        val lazyLoad = r.params.get("lazyload").contains("true")
        val hiddenPick = r.params.get("hiddenpick").contains("true")
        if (lazyLoad) {
          ctrl.lazyLoad(path, r.params("uuid"), r.params("fp"),
            r.params("reqnum").toInt, r.params("descriptor").toInt,
            r.params("mwpage_id"))
        } else if (hiddenPick) {
          ctrl.hiddenPick(path,
            r.params("reqnum").toInt, r.params("descriptor").toInt, r.params("fp"),
            r.params("mwpage_id"),
            r.params.get("search"), r.params.get("limit").flatMap(l => Helpers.tryo{l.toInt}))
        } else {
          val onlyTemplate = r.params.get("_t").contains("true")
          if (onlyTemplate) {
            val (redirect, templates) = ctrl.getTemplates(path, root(), parent, r.params.get("_loadto").orElse{
              r.params.get("_toid").map("#%s".format(_))
            })
            //Logger.debug("Get templates")
            //Logger.debug(templates)

            ctrl.render.json(
              redirect.map(s => JObject(JField("redirect", JString(s)))).getOrElse{
                JObject(
                  JField("templates", JObject(templates.map(t => JField(t._1, JString(t._2)))))
                )
              }
            ).status(200).toFuture
          } else {
            val isAjax = r.params.get("_ajax").contains("true")
            if (isAjax) {
              r.params.get("mwpage_id") match {
                case Some(renderVersion) =>
                  RenderVersion.doWith(renderVersion) {
                    ctrl.defaultAjax(r, path, root(), parent, r.params.get("_loadto").orElse{
                      r.params.get("_toid").map("#%s".format(_))
                    }, isAjax = true,
                      onlyData = r.params.get("_d").contains("true"), reqFromForm = r.params.get("_f").contains("true"),
                      hasLocalizations = r.params.get("_locs").flatMap(JSONParser.parse(_).map {
                        case JArray(arr) => arr.flatMap(_ match { case JString(s) => Some(s); case _ => None }).toList
                        case _ => Nil
                      }).getOrElse(Nil))
                  }
                case _ =>
                  ctrl.defaultAjax(r, path, root(), parent, r.params.get("_loadto").orElse{
                    r.params.get("_toid").map("#%s".format(_))
                  },isAjax = true,
                    onlyData = r.params.get("_d").contains("true"), reqFromForm = r.params.get("_f").contains("true"),
                    hasLocalizations = r.params.get("_locs").flatMap(JSONParser.parse(_).map {
                      case JArray(arr) => arr.flatMap(_ match { case JString(s) => Some(s); case _ => None }).toList
                      case _ => Nil
                    }).getOrElse(Nil))
              }
            } else {
              ctrl.defaultAjax(r, path, root(), parent)
            }
          }
        }
      })

      path._2.inner.foreach(_.addPaths(ctrl))
    })
  }

  private[server] def controller(): Controller = {
    val ctrl = new AjaxMapController(loadedFrom.get, SitemapMFactory.factory(this))
    addPaths(ctrl)

    ctrl
  }

  import net.aklabs.helpers.TimeHelpers._
  private[ajaxsite] def startWatch(): Unit = loadedFrom.foreach(sitemapPath => {
    ServerContext.system.scheduler.scheduleAtFixedRate(1 seconds, 1 seconds)(() => {
      try {
        val sitemapYmlPath = "%s/sitemap.yml".format(sitemapPath)
        val siteMapFile = new File(sitemapYmlPath)
        //Logger.debug("watch: " + siteMapFile.getAbsolutePath)
        //Logger.debug(SiteMap.sitemapFilesModTime)
        if (siteMapFile.lastModified() > SiteMap.sitemapFilesModTime(sitemapYmlPath)) {
          parsed = false

          SiteMap.rwl.writeLock().lock()
          val prevTimes = SiteMap.sitemapFilesModTime
          val prevSitemap = SiteMap.loadedSitemap

          SiteMap.sitemapFilesModTime = Map(sitemapYmlPath -> siteMapFile.lastModified())
          SiteMap.loadedSitemap = Map.empty
          SiteMap.cleanInnerTemplates()
          try {
            val json = Jckson.parseYaml(siteMapFile).asInstanceOf[JArray]
            val parsedMap = SiteMap.fromJson(json, None, None)
            paths = parsedMap.paths
            parseMap()
            onReloadListeners.foreach(_())
          } catch {
            case e: Throwable =>
              Logger.debug(prevSitemap)
              SiteMap.loadedSitemap = prevSitemap.map(kv => kv._1 -> "Sitemap error: %s".format(e.getMessage))
              SiteMap.sitemapFilesModTime = prevTimes.map(kv => kv._1 -> siteMapFile.lastModified())
              throw e
          } finally {
            SiteMap.rwl.writeLock().unlock()
          }
        } else {
          //TODO: include inner
          new File(sitemapPath).listFiles(new FilenameFilter {
            override def accept(dir: File, name: String): Boolean = {
              val fullPathName = new File(dir, name).getAbsolutePath.replaceAll("""\./""", "")
              val tName = name.replaceAll("""\.mustache""", "")
              SiteMap.loadedSitemap.contains(fullPathName) ||
              SiteMap.loadedInnerTemplates.contains(tName)
            }
          }).find(f => {
            if (f.exists() && SiteMap.sitemapFilesModTime.getOrElse(f.getAbsolutePath, {
              val t = f.lastModified()
              SiteMap.sitemapFilesModTime += f.getAbsolutePath -> t
              t
            }) < f.lastModified()) {
              parsed = false

              SiteMap.rwl.writeLock().lock()
              val prevTimes = SiteMap.sitemapFilesModTime
              val prevSitemap = SiteMap.loadedSitemap

              SiteMap.sitemapFilesModTime = Map(sitemapYmlPath -> siteMapFile.lastModified())
              SiteMap.loadedSitemap = Map.empty
              SiteMap.cleanInnerTemplates()
              try {
                parseMap()
                onReloadListeners.foreach(_())
              } catch {
                case e: Throwable =>
                  Logger.debug(prevSitemap)
                  SiteMap.loadedSitemap = prevSitemap.map(kv => kv._1 -> "Sitemap error: %s".format(e.getMessage))
                  SiteMap.sitemapFilesModTime = prevTimes.map(kv => kv._1 -> f.lastModified())
                  throw e
              } finally {
                SiteMap.rwl.writeLock().unlock()
              }
              true
            } else false
          })
        }
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    })(ServerContext.task_dispatcher)
  })
  private var onReloadListeners: Set[() => Unit] = Set.empty
  def onReload(f: () => Unit): Unit = {
    onReloadListeners += f
  }
}