package com.nn.mindweb.server.dataadapter

import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import com.nn.http.vars.RequestVar
import com.nn.http.{RenderVersion, RoundTripHandlerFunc}
import com.nn.mindweb.server.dataadapter.StructuredFormDataAdapter.varExtractRegexp
import com.nn.mindweb.server.dbgrpc.DbClient
import net.aklabs.db.dbapi.{MergeGroupedRule, SaveEl, SaveStatus, UserId}
import net.aklabs.helpers.JsonHelpers.{JNull, _}
import net.aklabs.helpers._
import org.pmw.tinylog.Logger

import scala.collection.{concurrent, mutable}
import scala.concurrent.Future
import scala.ref.{Reference, WeakReference}


class AngularServerStructure {
  //protected var fileUploadCallbacks: Map[String, (String, String, String, java.io.File) => PLCallMessage] = Map.empty
  private[dataadapter] var request_name: String = ""
  private[dataadapter] var model_struct: Map[String, JValue] = Map.empty
  private[dataadapter] var struct_vars: Map[String, (JValue, (Int, String))] = Map.empty
  private[dataadapter] var model: Map[String, JValue] = Map.empty
  private[dataadapter] var applying_model: Map[String, (Option[((String, String), Seq[Any])], JValue)] = Map.empty
  private[dataadapter] var hidden_struct: Map[String, JValue] = Map.empty
  private[dataadapter] var operationIdPattern: Option[String] = None
  private[dataadapter] var descriptorPaths: Map[(Int, Int), Seq[String]] = Map.empty

  private[dataadapter] var frontOperations: Set[String] = Set.empty
}
object AngularRequestServerStructure extends RequestVar[mutable.Map[String, AngularServerStructure]](mutable.Map.empty) {
  override protected def clearFunc(name: String): Unit = {
    Logger.debug("AngularRequestServerStructure is dead and cleaning up")
    Logger.debug(get.foreach(kv => {
      Logger.debug("Cleaning AngularRequestServerStructure: " + kv._1)
      Logger.debug("frontOperations: " + kv._2.frontOperations)
      StructuredFormDataAdapter.clearFrontOperations(kv._2.frontOperations.toSeq)
    }))
    super.clearFunc(name)
  }

  def getVars(path: String): Option[Map[String, (JValue, (Int, String))]] = {
    get.get(path).map(_.struct_vars)
  }
  def addVars(path: String, vars: Seq[(String, JValue, (Int, String))]): Unit = {
    get.get(path).foreach(s => {
      vars.foreach(v => s.struct_vars += v._1 -> (v._2, v._3))
    })
  }
}

case class CleanerHelper(rand: String = Helpers.nextFuncName)
object StructuredFormDataAdapter {

  def modelAndStructureForPage(data: JValue, withStruct: Boolean,
                               file_list: List[(Option[String], (String /*ObjectId*/, String))],
                               path: String, recordName: String, requestName: String, operationIdPattern: Option[String],
                               vars: Map[String, (JValue, (Int, String))],
                               hidden: Map[String, JValue],
                               useStructure: Option[AngularServerStructure] = None): (JValue, JValue) = {
    val structure = useStructure.getOrElse{
      val map = AngularRequestServerStructure.get
      val structure = map.getOrElse(path, {
        val structure = new AngularServerStructure()
        map.put(path, structure)
        structure
      })
      AngularRequestServerStructure.set(map)
      Logger.debug("modelAndStructureForPage: " + map)
      structure
    }

    Logger.debug(RenderVersion.get)
    Logger.debug("modelAndStructureForPage: " + AngularRequestServerStructure.get)
    Logger.debug(RenderVersion.get)
    Logger.debug("model and structure input data:" + data.toPrettyString)
    val (m, clm, s, dp) = modelAndStructure(data, withStruct, Nil, file_list,
      cl_h = Some(() => AngularCleanerHelper.get), recordName = Some(recordName),
      structureReference = Some(new WeakReference(structure)))

    structure.request_name = requestName
    structure.model_struct += recordName -> s
    structure.struct_vars = vars
    Logger.debug("VARS")
    Logger.debug(vars)
    structure.model += recordName -> clm
    structure.hidden_struct = hidden.map(kv => kv._1 -> {
      val hasPick = kv._2.has("pick")
      if (hasPick) kv._2.asInstanceOf[ObjectNode].put("pick", true)
      else kv._2
    })
    structure.operationIdPattern = operationIdPattern
    structure.descriptorPaths = dp

    Logger.debug("hidden_struct")
    Logger.debug(structure.hidden_struct)

    (m, s)
  }

  def modelAndStructure(data: JValue, withStruct: Boolean): (JValue, JValue, JValue, Map[(Int, Int), Seq[String]]) = {
    modelAndStructure(data, withStruct, Nil, Nil)
  }





  protected object AngularCleanerHelper extends RequestVar[CleanerHelper](CleanerHelper()) {
    override lazy val __nameSalt: String = Helpers.nextFuncName
  }

  private case class RefStructure(single: Boolean, possibleLinks: JObject, available_p: String,
                                  parent: Option[RefStructure]) {
  }
  private def genRefStructure(jv: JObject, parent: Option[RefStructure]): (RefStructure, Int) = {
    var single = false
    var possibleLinks: JObject = JObject(Nil)
    var available_p = ""
    var recursion = -1
    jv.children.foreach {
      case f: JField =>
        if (f.name == "rec") recursion = f.value.asInstanceOf[JInt].numberValue().intValue()
        else if (f.name == "single") single = f.value.asInstanceOf[JBool].booleanValue()
        else if (f.name == "available_p") {
          if (f.value != JNull()) available_p = f.value.elements().toString
        }
        else if (f.name == "possible_links") possibleLinks = f.value.asInstanceOf[JObject]
      case _ =>
    }
    //Logger.debug("genRefStructure: " + available_p)
    if (recursion > -1) {
      val rec = recursion + 1
      var recStr = parent
      //println("HAS RECURSION AND HAS PARENT: " + recursion + " : " + recStr)
      while (recursion > 0 && recStr.isDefined) {
        recStr = recStr.flatMap(_.parent)
        recursion -= 1
      }
      recStr.getOrElse(RefStructure(single, possibleLinks, available_p, parent)) -> rec
    }
    else RefStructure(single, possibleLinks, available_p, parent) -> 0
  }

  def transformToWebModel(data: JValue,
                          file_list: List[(Option[String], (String /*ObjectId*/, String))],
                          updateAtDescriptorPath: Option[String] = None
                         ): (JValue, JValue) = {
    val structure = updateAtDescriptorPath.flatMap(p => AngularRequestServerStructure.get.get(p))
    Logger.debug("transformToWebModel: " + updateAtDescriptorPath)
    val (m, clm, _, dp) = modelAndStructure(data, withStruct = false, Nil, file_list,
      cl_h = Some(() => AngularCleanerHelper.get), recordName = None)

    structure.foreach(s => {
      dp.foreach(descrP => {
        s.descriptorPaths += descrP
      })
    })
    m -> clm
  }
  def addToModelByPath(clm: Seq[JValue], modelPath: String, pathInModel: String, requestDescriptor: (Int, Int)): Unit = {
    AngularRequestServerStructure.get.get(modelPath).foreach(structure => {
      structure.descriptorPaths.get(requestDescriptor).foreach(structPath => {
        val splittedFullPath = pathInModel.split("""\.""")
        val splittedPath = splittedFullPath.tail.toSeq.map(p => {
          val splits = p.split("#")
          if (splits.size == 2 && splits(1).nonEmpty) {
            splits(0).trim -> Some(splits(1).trim)
          } else if (splits.size <= 2) {
            splits(0).trim -> None
          } else {
            throw new Exception("Не корректный путь: %s".format(pathInModel))
          }
        })

        Logger.debug(splittedPath.map(_._1) + " == " + structPath.dropRight(1))
        if (splittedPath.map(_._1) == structPath.dropRight(1)) {
          structure.model.get(splittedFullPath.head).foreach(model => {
            var currentLeaf = model
            Logger.debug("model: " + model)
            Logger.debug(splittedPath)
            splittedPath.foreach(p => {
              currentLeaf \ p._1 match {
                case JArray(arr) if p._2.nonEmpty =>
                  val jvlink_id = Helpers.tryo{JInt(p._2.get.toInt)}.getOrElse(JString(p._2.get))
                  arr.find(_ \ "jvlink_id" == jvlink_id) match {
                    case Some(o) => currentLeaf = o
                    case _ => throw new Exception("Не корректный путь: %s".format(pathInModel))
                  }
                case o: JObject if p._2.isEmpty =>
                  currentLeaf = o
                case _ =>
                  throw new Exception("Не корректный путь: %s".format(pathInModel))
              }
            })
            val lastFieldName = structPath.last
            currentLeaf.findValue(lastFieldName) match {
              case a: JArray =>
                Logger.debug(currentLeaf.toString + " : " + lastFieldName)

                val obj = currentLeaf.asInstanceOf[ObjectNode]
                clm.foreach(a.add(_))
                obj.set(lastFieldName, a)
                lastFieldName
              case _ =>
                throw new Exception("Не корректный тип последнего поля '%s' в пути: %s".format(
                  lastFieldName, pathInModel
                ))
            }
          })

          Logger.debug("updated model")
          Logger.debug(structure.model.get(splittedFullPath.head))
        }
      })
    })
  }
  private def modelAndStructure(data: JValue, withStruct: Boolean,
                                path: List[String],
                                file_list: List[(Option[String], (String /*ObjectId*/, String))],
                                cl_h: Option[() => CleanerHelper] = None,
                                root_field: String = "", recordName: Option[String] = None,
                                skip_r_and_str: Boolean = false,
                                fileUploadCallbacks: Map[String, (String, String, String, java.io.File) => JValue] = Map.empty,
                                structureReference: Option[Reference[AngularServerStructure]] = None,
                                ref_str: Option[RefStructure] = None): (JValue, JValue, JValue, Map[(Int, Int), Seq[String]]) = {
    //TODO: add file download
    //cl_h.foreach(FilesFunctionMap.add_files_list(_, file_list))
    var descriptorPaths: Map[(Int, Int), Seq[String]] = Map.empty

    data match {
      case obj: JObject =>
        val flds = obj.children.collect{case x: JField => x}
        var cleanedFields: List[JField] = recordName.map(recn => JField("__recn", JString(recn)) :: Nil).getOrElse(Nil)
        var cleanedModelFields: List[JField] = Nil
        var structFields: List[JField] = Nil
        var fname = ""
        flds.foreach(fld => fld.value match {
          //case _ if (fld.name == "_filter_fields_") =>
          case _ if fld.name == "jvlink_param" =>
            Logger.debug("jvlink_param field add")
            cleanedFields ::= fld
            cleanedModelFields ::= fld
          case rel_p if fld.name == "jvrel_prop" =>
            cleanedFields ::= JField(fld.name, JObject( rel_p.children.flatMap(_ match {
              case fld: JField =>
                var dateFields: List[JField] = Nil
                var enumFields: List[JField] = Nil
                var optFields: List[JField] = Nil
                var date_field = false
                var enum_field = false
                var opt_enum = false
                var opt_field = false
                var preCleaned: JValue = JObject(Nil)

                fld.value match {
                  case obj: JObject =>
                    fld.value.children.foreach {
                      case f: JField =>
                        if (f.name == "jvdate_field") date_field = true
                        else if (f.name == "jvenum_field") enum_field = true
                        else if (f.name == "jvopt_enum") opt_enum = true
                        else if (f.name == "jvopt_field") opt_field = true
                        else if (date_field) {
                          if (f.name == "dval") {
                            dateFields ::= f
                          } else if (f.name == "date_format") {
                            dateFields ::= f
                          }
                        }
                        else if (enum_field) {
                          if (f.name == "e_val") enumFields ::= f
                          else if (f.name == "enm") enumFields ::= f //JField(f.name, JString("av_p_vars." + f.value.asInstanceOf[JString].s))
                        }
                        else if (opt_field) {
                          if (f.name == "o_val") optFields ::= f
                          else if (f.name == "opt") optFields ::= f //JField(f.name, JString("av_p_vars." + f.value.asInstanceOf[JString].s))
                          else if (f.name == "sngl") optFields ::= f
                        } else {
                          //basic field
                          preCleaned = f.value
                        }
                      case _ =>
                    }
                  case _ => preCleaned = fld.value
                }

                if (dateFields.nonEmpty) {
                  dateFields ::= JField("jvdate_field", JBool(true))
                  preCleaned = JObject(dateFields)
                }
                if (enumFields.nonEmpty) {
                  enumFields ::= JField("jvenum_field", JBool(true))
                  preCleaned = JObject(enumFields.map(f => if (f.name == "enm") {
                    JField(f.name, JString(f.value.asInstanceOf[JString].textValue()))
                  } else f))
                  if (opt_enum) enumFields ::= JField("jvopt_enum", JBool(true))
                }
                if (optFields.nonEmpty) {
                  optFields ::= JField("jvopt_field", JBool(true))
                  preCleaned = JObject(optFields.map(f => if (f.name == "opt") {
                    JField(f.name, JString(f.value.asInstanceOf[JString].textValue()))
                  } else f))
                }

                Some(JField(fld.name, preCleaned))
              case _ => None
            }) ))

            //cleanedFields :::= preCleaned
            //cleanedFields ::= JField(fld.name, preCleaned)
          case link: JObject =>
            //do link cleaning
            var single = link \ "single" match {case JBool(true) => true; case _ => false}
            var constant = false
            var hidden = false
            var can_create = false
            var can_read = false
            var can_write = false
            var pick_descr: Option[Int] = None
            var pick_req_num: Option[Int] = None
            var pick_has_search: Option[Boolean] = None
            var editable = false
            var key_field: Option[String] = None
            var possibleLinks: JObject = JObject(Nil)
            var available_p = link \ "available_p" match {case JString(p) => p; case _ => ""}
            val parametric = available_p.nonEmpty ||
              (link \ "parametric" match {case JBool(b) => b; case _ => false})
            var hasAny = false
            var showAnyParam = false
            var mainParam: Option[String] = None
            var preCleaned: JValue = JObject(Nil)
            var cleanModel: JValue = JObject(Nil)
            var fileFields: List[JField] = Nil
            var fileStructFields: List[JField] = Nil
            var dateFields: List[JField] = Nil
            var enumFields: List[JField] = Nil
            var optFields: List[JField] = Nil
            var file_field = false
            var date_field = false
            var enum_field = false
            var opt_enum = false
            var opt_field = false
            var rel_prop_f: Option[JField] = None
            var rel_prop_un: Option[JField] = None
            var distinct_m: Option[JField] = None
            var lazy_desc: Option[Int] = None
            var lazy_req_num: Option[Int] = None
            var lazy_fields: Option[Boolean] = None

            Logger.debug(fld.name + " -> " + single + " : " + link)
            link.children.foreach {
              case f: JField =>
                if (f.name == "single") {}
                else if (f.name == "constant") constant = f.value.asInstanceOf[JBool].booleanValue()
                else if (f.name == "hasAny") hasAny = f.value.asInstanceOf[JBool].booleanValue()
                else if (f.name == "showAnyParam") showAnyParam = f.value.asInstanceOf[JBool].booleanValue()
                else if (f.name == "mainParam") mainParam = Some(f.value.elements().toString)
                else if (f.name == "possible_links") {
                  possibleLinks = f.value.asInstanceOf[JObject]
                }
                else if (f.name == "rel_props_filter") rel_prop_f = Some(f)
                else if (f.name == "rel_prop_unique") rel_prop_un = Some(f)
                else if (f.name == "distinct_merge") distinct_m = Some(f)
                else if (f.name == "hidden") hidden = f.value.asInstanceOf[JBool].booleanValue()
                else if (f.name == "jv_can_create") can_create = f.value.asInstanceOf[JBool].booleanValue()
                else if (f.name == "jv_can_read") can_read = f.value.asInstanceOf[JBool].booleanValue()
                else if (f.name == "jv_can_write") can_write = f.value.asInstanceOf[JBool].booleanValue()
                else if (f.name == "parametric" || f.name == "available_p") {} //ignore
                else if (f.name == "pickDescriptor") pick_descr = Some(f.value match {case JInt(ld) => ld.toInt})
                else if (f.name == "pickReqNum") pick_req_num = Some(f.value match {case JInt(ld) => ld.toInt})
                else if (f.name == "pickHasSearch") pick_has_search = Some(f.value match {case JBool(b) => b})
                else if (f.name == "editable") editable = f.value.asInstanceOf[JBool].booleanValue()
                else if (f.name == "key_field") key_field = Some(f.value.asInstanceOf[JString].textValue())
                else if (f.name == "jvfile_field") file_field = true
                else if (f.name == "jvdate_field") date_field = true
                else if (f.name == "jvenum_field") enum_field = true
                else if (f.name == "jvopt_enum") opt_enum = true
                else if (f.name == "jvopt_field") opt_field = true
                else if (f.name == "lazyDescriptor") lazy_desc = Some(f.value match {case JInt(ld) => ld.toInt})
                else if (f.name == "lazyReqNum") lazy_req_num = Some(f.value match {case JInt(ld) => ld.toInt})
                else if (f.name == "lazyFields") lazy_fields = Some(f.value match {case JBool(b) => b})
                else if (file_field) {
                  if (f.name == "d" || f.name == "c" || f.name == "bg" || f.name == "es" || f.name == "all_ext") {
                    fileStructFields ::= f
                  } else {
                    if (f.name == "fname") {
                      fname = f.value.elements.toString
                    }
                    fileFields ::= f
                  }
                }
                else if (date_field) {
                  if (f.name == "dval") {
                    dateFields ::= f
                  } else if (f.name == "date_format") {
                    dateFields ::= f
                  }
                }
                else if (enum_field) {
                  if (f.name == "e_val") enumFields ::= f
                  else if (f.name == "enm") enumFields ::= JField(f.name, JString(f.value.asInstanceOf[JString].textValue()))
                }
                else if (opt_field) {
                  if (f.name == "o_val") optFields ::= f
                  else if (f.name == "opt") optFields ::= JField(f.name, JString(f.value.asInstanceOf[JString].textValue()))
                  else if (f.name == "sngl") optFields ::= f
                }
                else {
                  val (rstr, pth) = if (ref_str.isEmpty) (Some(RefStructure(single, possibleLinks, available_p, None)), path) else {
                    val pth = path :+ fld.name
                    var cur_str: Option[RefStructure] = ref_str
                    //println("REF STRUCTURE NOT EMPTY: " + pth)
                    pth.foreach(p => {
                      cur_str = cur_str.flatMap(_.possibleLinks.children.collectFirst {
                        case x: JField if x.name == p => x
                      }.map(f =>
                        genRefStructure(f.value.asInstanceOf[JObject], cur_str)._1))
                      //println("cur_str")
                      //println(cur_str)
                    })
                    cur_str.foreach(str => {
                      single = str.single
                      possibleLinks = str.possibleLinks
                      available_p = str.available_p
                    })
                    (ref_str, pth)
                  }
                  Logger.debug("FIELD NAME! " + f.name + " : " + f.value)
                  val child = f.value.asInstanceOf[JArray].arr.toList.map(obj => {
                    val ms = modelAndStructure(JObject(obj.asInstanceOf[JObject].children.flatMap(_ match {
                      case x: JField if x.name != "jvlink_props" && (x.name != "jvlink_param" || parametric) => Some(x)
                      case _ => None
                    })), withStruct, pth, Nil, cl_h, if (root_field.isEmpty) fld.name else root_field, recordName, skip_r_and_str, fileUploadCallbacks, structureReference, rstr)
                    ms._4.foreach(dp => descriptorPaths += dp._1 -> dp._2)
                    ms._1 -> ms._2
                  })

                  if (link \ "parametric" match {case JBool(true) => true; case _ => false}) {
                    Logger.debug("IN PARAMETRIC: " + child.map(_._1))
                    Logger.debug(child.map(_._2))
                    preCleaned = JObject(child.map(_._1).map(obj => {
                      var param = ""
                      JObject(obj.children.toList.flatMap(_ match {
                        case x: JField if x.name != "jvlink_id" =>
                          if (x.name == "jvlink_param") {
                            param = (x.value \ "param").asText()
                            Logger.debug("SET PARAM: " + param)
                            None
                          } else Some(x)
                        case _ => None
                      })) -> param
                    }).map(x => {
                      Logger.debug("Add parametric link: " + x)
                      JField(x._2, x._1)
                    }))
                    cleanModel = JObject(child.map(_._2).map(obj => {
                      var param = ""
                      JObject(obj.children.toList.flatMap(_ match {
                        case x: JField if x.name != "jvlink_id" =>
                          if (x.name == "jvlink_param") {
                            param = (x.value \ "param").asText()
                            None
                          } else Some(x)
                        case _ => None
                      })) -> param
                    }).map(x => JField(x._2, x._1)))
                  } else {
                    Logger.debug("Check single: " + single + " : " + link)
                    preCleaned = if (single) child.headOption.map(_._1).getOrElse(possibleLinks).asInstanceOf[JObject] else JArray(child.map(_._1).toList)
                    cleanModel = if (single) child.headOption.map(_._2).getOrElse(possibleLinks).asInstanceOf[JObject] else JArray(child.map(_._2).toList)
                  }
                }
              case _ =>
            }

            val pth = path :+ fld.name
            var cur_str: Option[RefStructure] = ref_str
            var rec_pth: List[String] = Nil
            pth.foreach(p => {
              val _cur_str = cur_str.flatMap(_.possibleLinks.children.collectFirst{
                case x: JField if x.name == p => x}.map(f =>
                genRefStructure(f.value.asInstanceOf[JObject], cur_str)))
              cur_str = _cur_str.map(_._1)
              _cur_str.foreach(cstr => {
                rec_pth = rec_pth.splitAt(cstr._2)._2
              })
              rec_pth ::= p
            })
            rec_pth = rec_pth.reverse

            val fv_str_arr = JArray({if (root_field.isEmpty) rec_pth else root_field :: rec_pth}.map(JString(_)))

            if (dateFields.nonEmpty) {
              dateFields ::= JField("jvdate_field", JBool(true))
              val obj = JObject(dateFields)
              preCleaned = obj
              cleanModel = obj
            }
            if (enumFields.nonEmpty) {
              enumFields ::= JField("jvenum_field", JBool(true))
              enumFields.find(_.name == "e_val") match {
                case Some(e_val) =>
                  preCleaned = JObject(
                    e_val ::
                    JField("jvenum_field", JBool(true)) ::
                    JField("jv_struct", fv_str_arr) :: Nil)
                  cleanModel = JObject(e_val :: JField("jvenum_field", JBool(true)) :: Nil)
                case _ =>
                  val obj = JObject(enumFields)
                  preCleaned = obj
                  cleanModel = obj
              }
              if (opt_enum) enumFields ::= JField("jvopt_enum", JBool(true))

              //Logger.debug("preCleaned enum: " + preCleaned.toPrettyString)
            }
            if (optFields.nonEmpty) {
              optFields ::= JField("jvopt_field", JBool(true))
              val obj = JObject(optFields)
              preCleaned = obj
              cleanModel = obj
            }

            if (fileFields.nonEmpty) {
              if (withStruct) {
                //println("CREATE FILE FIELD: " + rec_pth + " " + path + " " + root_field)
                fileFields ::= JField("upload_id", getUploadId(JObject(List(
                  JField("rend_v", JString(RenderVersion.get)),
                  JField("path", fv_str_arr))
                ), cl_h, fileUploadCallbacks, structureReference).arr.head)
              }
              fileFields ::= JField("jvfile_field", JBool(true))
              val obj = JObject(fileFields)
              preCleaned = obj
              cleanModel = obj
            }


            val empty_arr = if (link.children.collect{case f: JField if f.name != "jv_can_read" &&
              f.name != "jv_can_write" &&
              f.name != "jv_can_create" => f}.isEmpty) {
              val pth = path :+ fld.name
              var cur_str: Option[RefStructure] = ref_str
              pth.foreach(p => {
                cur_str = cur_str.flatMap(_.possibleLinks.children.collectFirst{
                  case x: JField if x.name == p => x}.map(f =>
                  genRefStructure(f.value.asInstanceOf[JObject], cur_str)._1))
              })

              cur_str.exists(cs => cs.possibleLinks.children.nonEmpty && !cs.single && cs.available_p.isEmpty)
            } else {
              link\ "links" match {
                case JArray(arr) => arr.isEmpty && !single && !parametric /*parametric.isEmpty*/
                case null =>
                  !single && !parametric /*parametric.isEmpty*/ && !file_field && !date_field && !enum_field && !opt_field
                case _ => true
              }
            }

            Logger.debug(fld.name + " -> " + preCleaned + " : " + empty_arr + " : " + enum_field)

            val rights_obj = JObject(
              JField("can_read", JBool(can_read)) ::
                JField("can_write", JBool(can_write)) ::
                JField("can_create", JBool(can_create)) :: Nil
            )
            if (empty_arr) {
              cleanedModelFields :::= mainParam.map(mp => JField(fld.name + "_main_param", JString(mp)) :: Nil).getOrElse(Nil) :::
                JField(fld.name, JArray(Nil)) :: Nil
              cleanedFields :::= mainParam.map(mp => JField(fld.name + "_main_param", JString(mp)) :: Nil).getOrElse(Nil) :::
                JField(fld.name, JArray(Nil)) ::
                (if (!skip_r_and_str) {
                  JField(fld.name + "_jv_struct", fv_str_arr) ::
                    JField(fld.name + "_jv_rights", rights_obj) :: Nil
                } else Nil)
              /*
              if (single) {
                cleanedModelFields :::= JField(fld.name,
                  JObject(mainParam.map(mp => JField("main_param", JString(mp)) :: Nil).getOrElse(Nil))) ::
                  cleanedFields
                cleanedFields ::= JField(fld.name, JObject(mainParam.map(mp => JField("main_param", JString(mp)) :: Nil).getOrElse(Nil) :::
                  recordName.map(recn => JField("__recn", JString(recn)) :: Nil).getOrElse(Nil) :::
                  (if (!skip_r_and_str) {
                    JField("jv_struct", fv_str_arr) :: JField("jv_rights", rights_obj) :: Nil
                  } else Nil)))
              } else {
                cleanedModelFields :::= JField(fld.name,
                  JObject(mainParam.map(mp => JField("main_param", JString(mp)) :: Nil).getOrElse(Nil))) ::
                  cleanedFields
                cleanedFields :::= mainParam.map(mp => JField(fld.name + "_main_param", JString(mp)) :: Nil).getOrElse(Nil) :::
                  JField(fld.name, JArray(Nil)) ::
                  (if (!skip_r_and_str) {
                    JField(fld.name + "_jv_struct", fv_str_arr) ::
                      JField(fld.name + "_jv_rights", rights_obj) :: Nil
                  } else Nil)
              }
               */
            }
            else {
              cleanedModelFields :::= { cleanModel match {
                case JArray(arr) =>
                  mainParam.map(mp => JField(fld.name + "_main_param", JString(mp)) :: Nil).getOrElse(Nil) :::
                  JField(fld.name, JArray(arr.toList)) :: Nil
                case obj: JObject => JField(fld.name, JObject(mainParam.map(mp => JField("main_param", JString(mp)) :: Nil).getOrElse(Nil) :::
                  obj.children.toList.collect{case f: JField => f})) :: Nil
                case _ => Nil
              }}
              cleanedFields :::= { preCleaned match {
                case JArray(arr) =>
                  mainParam.map(mp => JField(fld.name + "_main_param", JString(mp)) :: Nil).getOrElse(Nil) :::
                  JField(fld.name, JArray(arr.toList)) ::
                  (if (!skip_r_and_str) {
                    JField(fld.name + "_jv_struct", fv_str_arr) ::
                      JField(fld.name + "_jv_rights", rights_obj) :: Nil
                  } else Nil)
                case obj: JObject => JField(fld.name, JObject(mainParam.map(mp => JField("main_param", JString(mp)) :: Nil).getOrElse(Nil) :::
                  recordName.map(recn => JField("__recn", JString(recn)) :: Nil).getOrElse(Nil) :::
                  (if (!date_field && !enum_field && !opt_field && !skip_r_and_str) {
                    JField("jv_struct", fv_str_arr) :: Nil
                  } else Nil) :::
                  (if (!file_field && !date_field && !enum_field && !opt_field && !skip_r_and_str) {
                    JField("jv_rights", rights_obj) :: obj.children.toList.collect{case f: JField => f}
                  }
                  else obj.children.toList.collect{case f: JField => f}) )) :: Nil
                case _ => Nil
              } }
            }

            if (withStruct) {
              //println("WITH STRUCT: " + fld.name + " : " + possibleLinks)
              def fixPossibleLinks(jobj: JObject, pth: Seq[String]): JObject = {
                val enum_f = jobj \ "jvenum_field" match {case JBool(true) => true; case _ => false}
                val opt_f = jobj \ "jvopt_field" match {case JBool(true) => true; case _ => false}
                val pdesc_v = jobj \ "pickDescriptor" match {case JInt(ld) => Some(ld.toInt); case _ => None}
                val preq_v = jobj \ "pickReqNum" match {case JInt(ld) => Some(ld.toInt); case _ => None}
                val psearch_v = jobj \ "pickHasSearch" match {case JBool(b) => Some(b); case _ => None}
                val ldesc_v = jobj \ "lazyDescriptor" match {case JInt(ld) => Some(ld.toInt); case _ => None}
                val lreq_v = jobj \ "lazyReqNum" match {case JInt(ld) => Some(ld.toInt); case _ => None}
                val lazy_fields = jobj \ "lazyFields" match {case JBool(b) => b; case _ => false}
                JObject(jobj.children.collect{case x: JField =>
                  if (x.name == "pickDescriptor") {
                    descriptorPaths += (pdesc_v.get, preq_v.get) -> pth
                    Some(JField("pick", JObject(List(
                      JField("d", JInt(pdesc_v.get)),
                      JField("n", JInt(preq_v.get))
                    ) ::: psearch_v.map(ps => JField("search", JBool(ps))).toList)))
                  } else if (x.name == "pickReqNum") None
                  else if (x.name == "pickHasSearch") None
                  else if (x.name == "lazyDescriptor") {
                    descriptorPaths += (ldesc_v.get, lreq_v.get) -> pth
                    Some(JField("lazy", JObject(List(
                      JField("d", JInt(ldesc_v.get)),
                      JField("n", JInt(lreq_v.get)),
                      JField("fields", JBool(lazy_fields))
                    ))))
                  } else if (x.name == "lazyReqNum") None
                  else if (x.name == "lazyFields") None
                  else
                  x.value match {
                    //case _ if (x.name == "_filter_fields_") => None
                    case jobj: JObject => Some(JField(x.name, {
                      val path = if (x.name != "possible_links") pth :+ x.name else pth
                      fixPossibleLinks(jobj, path)
                    }))
                    case JString(s) if x.name == "available_p" ||
                      (x.name == "enm" && enum_f) || (x.name == "opt" && opt_f) =>
                      Some(JField(x.name, JString(s)))
                    case null | _: JNull if x.name == "available_p" => None
                    case _: JBool if x.name == "jv_can_read" || x.name == "jv_can_write" || x.name == "jv_can_create" => None
                    case _ => Some(x)
                  }
                }.flatten)
              }

              structFields ::= JField(fld.name, JObject(
                if (enum_field) enumFields.filter(_.name != "e_val")
                else if (opt_field) optFields.filter(_.name != "o_val")
                else {
                  lazy_desc.flatMap(ld => lazy_req_num.map(num => {
                    descriptorPaths += (ld, num) -> (path :+ fld.name)
                    JField("lazy", JObject(List(
                      JField("d", JInt(ld)),
                      JField("n", JInt(num))
                    ) ::: lazy_fields.map(lf => JField("fields", JBool(lf))).toList))
                  })).toList :::
                  JField("single", JBool(single)) ::
                    {if (single) JField("constant", JBool(constant)) :: Nil else Nil} :::
                    //JField("can_create", JBool(can_create)) ::
                    {if (hidden) {
                      JField("hidden", JBool(hidden)) ::
                        JField("editable", JBool(editable)) ::
                        pick_descr.flatMap(pd => pick_req_num.map(num => {
                          descriptorPaths += (pd, num) -> (path :+ fld.name)
                          JField("pick", JObject(List(
                            JField("d", JInt(pd)),
                            JField("n", JInt(num))
                          ) ::: pick_has_search.map(hs => JField("search", JBool(hs))).toList))
                        })).toList :::
                        key_field.map(kf => JField("key_field", JString(kf))).toList
                    } else Nil/*{
                      JField("can_read", JBool(can_read)) ::
                      JField("can_write", JBool(can_write)) :: Nil
                    }*/} :::
                    {if (file_field) {
                      JField("jvfile_field", JBool(file_field)) :: fileStructFields
                    } else Nil} :::
                    {
                      if (hasAny) JField("hasAny", JBool(true)) :: Nil else Nil
                    } :::
                    {
                      if (showAnyParam) JField("showAnyParam", JBool(true)) :: Nil else Nil
                    } :::
                    {if (!available_p.isEmpty) List(JField("available_p", JString(available_p))) else Nil} :::
                    JField("possible_links", fixPossibleLinks(possibleLinks, path)) :: Nil
                } ::: rel_prop_f.map(_ :: Nil).getOrElse(Nil) :::
                  rel_prop_un.map(_ :: Nil).getOrElse(Nil) ::: distinct_m.map(_ :: Nil).getOrElse(Nil)
              ))
            }
          case x =>
            Logger.debug("Simple field: " + fld.name)
            cleanedFields ::= JField(fld.name, x)
            cleanedModelFields ::= JField(fld.name, x)
        })

        (JObject(cleanedFields), JObject(cleanedModelFields), {
          if (structFields.isEmpty) JNull() else JObject(structFields)
        }, descriptorPaths)
      case _ => (data, data, JNull(), descriptorPaths)
    }
  }

  def getUploadId(in: JValue, progAct: Option[() => CleanerHelper],
                  fileUploadCallbacks: Map[String, (String, String, String, java.io.File) => JValue],
                  snippetReference: Option[Reference[AngularServerStructure]]): JArray = {
    val repeats = in\"num" match {
      case JInt(i) if i > 0 => i.toInt
      case _ => 1
    }
    /*
    val upload_ids = (1 to repeats).map(_ => {
      var upload_id = ""
      var renderV = RenderVersion.get
      upload_id = fmapFunc(BinFuncHolder(fp => {
        in\"rend_v" match {case JString(rv) => {
          if (rv != renderV) renderV = rv
          AngularUploads += (rv, (upload_id -> fp))
        } case _ =>}
      })) { name => {
        progAct.foreach(l => {
          val pl = new ProgressListener(name)
          FileProgresses += (name -> pl, l._2())
          pl.addListener(l._1)
          in\"path" match {
            case JArray(arr) => {
              val str_arr = arr.collect{case JString(str) => str}
              if (str_arr.length == arr.length) {
                pl.setOnFinish((contentType: String, fileName: String, file: java.io.File) => {
                  pl.listeners.headOption.foreach(listenerActor => {
                    snippetReference.flatMap(_.get).foreach(ref => {
                      //println("uploading finished", str_arr)
                      //println(ref.model_struct)
                      ref.model_struct.map(model_str => {
                        var v = model_str._2
                        var next_v: JValue = JNothing
                        str_arr.foreach(p => {
                          next_v = v\p
                          v = next_v\"possible_links"
                          //println(p, next_v)
                        })
                        model_str._1 -> next_v
                      }).find(_._2 != JNothing) match {
                        case Some(field_struct) => {
                          println("GOT FIELD STRUCT: " + field_struct._2)
                          if (field_struct._2\"is_image" match {case JBool(true) => true; case _ => false}) {
                            val es = field_struct._2\"es" match {
                              case exactSize: JObject => {
                                exactSize\"w" match {case JInt(w) => exactSize\"h" match {case JInt(h) => Some(w.toInt, h.toInt)}; case _ => None}
                              }
                              case _ => None
                            }
                            val allExt = field_struct._2\"all_ext" match {
                              case allowedExtentions: JArray => {
                                allowedExtentions.arr.collect{case JString(s) => s}
                              }
                              case _ => Nil
                            }
                            val inBox = field_struct._2\"d" match {
                              case JArray(arr) => {
                                arr.flatMap(d => {
                                  d\"w" match {case JInt(w) => d\"h" match {case JInt(h) =>
                                    Some(w.toInt, h.toInt, d\"m" match {case JString(s) => Some(s); case _ => None})
                                  }; case _ => None}
                                })
                              }
                              case _ => Nil
                            }
                            val forceCompress = field_struct._2\"c" match {case JBool(true) => true; case _ => false}
                            val bg = field_struct._2\"bg" match {
                              case JArray(arr) => {
                                if (arr.size == 3) {
                                  arr(0) match {
                                    case JInt(r) => arr(1) match {
                                      case JInt(g) => arr(2) match {
                                        case JInt(b) => Some((r.toByte, g.toByte, b.toByte))
                                        case _ => None
                                      }
                                      case _ => None
                                    }
                                    case _ => None
                                  }
                                } else None
                              }
                              case _ => None
                            }

                            //logger.debug("File was uploaded: " + file)
                            val barr = IOUtils.toByteArray(new FileInputStream(file))

                            val bridge = MyMainActorSystem.system.actorOf(akka.actor.Props[ImageBridgeActor], name = "ui_image_bridgeactor" + Helpers.randomString(50))
                            bridge ! SetFileUploadListener(listenerActor)

                            (ImageProcessingSharder.manager ! ProcessImage(upload_id, barr, fileName,
                              es, allExt, None, inBox, forceCompress, bg,
                              MessagingUserId.getUserId(SecurerUser.currentUserId)))(bridge)
                          }
                        }
                        case _ =>
                      }
                    })
                  })
                  fileUploadCallbacks.get(str_arr.foldLeft("")(_ + "/" + _)).foreach(func => {
                    S.session match {
                      case Full(sess) => sess.executeInScope(Empty, renderV) {
                        pl.listeners.foreach(_ ! func(pl.name, contentType, fileName, file))
                      }
                      case _ => pl.listeners.foreach(_ ! func(pl.name, contentType, fileName, file))
                    }
                  })
                })
              }
            }
            case _ =>
          }
        })
        name
      } }
      upload_id
    }).toList
     */

    //JArray( upload_ids.map(JString(_)) )

    JArray(Nil)
  }



  def saveDirect(recordName: String, uuid: String, data: JValue,
                 structure: AngularServerStructure, mergeRules: Seq[MergeGroupedRule],
                 partialSavePathString: Option[String],
                 func: RoundTripHandlerFunc,
                 forceUser: Option[UserId] = None,
                 keepUniqueValues: Seq[String] = Nil): Future[SaveStatus] = {
    val sequentialOpKey = DbClient.getSequentialOperationId(structure.operationIdPattern,
      structure.request_name,
      structure.model_struct.keys.toList,
      List(recordName -> uuid))

    Logger.debug(sequentialOpKey)

    val frontOperationId = UUID.randomUUID().toString
    //TODO: Создаём временный actor с ключём opId
    frontOperations += frontOperationId -> func
    structure.frontOperations += frontOperationId
    //по opId принимаем данные об ошибках во время сохранения
    DbClient.save(sequentialOpKey, frontOperationId, structure.request_name, Seq(
      SaveEl(recordName, uuid, data.toString)
    ), partialSavePathString, mergeRules, forceUser, keepUniqueValues)
  }
  def save(data: JsonNode, func: RoundTripHandlerFunc): Unit = {
    Logger.debug("Call roundtrip save: " + data.toPrettyString)

    data \ "path" match {
      case JString(path) =>
        path.split(";").foreach(path => {
          if (AngularRequestServerStructure.get.get(path).map(serverStructure => {
            var gotFailures = false
            val d = data \ "data"
            val partialSavePathString = d \ "__partialSave" match {
              case JString(psP) => Some(psP)
              case _ => None
            }
            val partialSavePath = partialSavePathString match {
              case Some(s) =>
                val path = s.split("""\.""")
                val h = path.head.split('#')
                if (h.length == 2 && h(1).nonEmpty) {
                  Some(
                    (h(0) -> h(1)) -> path.tail.toSeq.map(p => {
                      val splits = p.split("#")
                      if (splits.size == 2 && splits(1).nonEmpty) {
                        splits(0).trim -> Some(splits(1).trim)
                      } else if (splits.size <= 2) {
                        splits(0).trim -> None
                      } else {
                        Logger.debug("Не корректный путь 1")
                        func.failure("Не корректный путь %s".format(s))
                        gotFailures = true
                        "" -> None
                      }
                    })
                  )
                } else {
                  Logger.debug("Не корректный путь 2")
                  func.failure("Не корректный путь %s".format(s))
                  gotFailures = true
                  None
                }
              case _ =>
                None
            }

            if (!gotFailures) {
              val preparedSaveInputs = d.children.flatMap(f => {
                if (f.name != "__partialSave") {
                  val recordName = partialSavePath.map(_._1._1).getOrElse(f.name)
                  val inputReady = Box(serverStructure.model.find(_._1 == recordName)).flatMap(cachedValue => {
                    Logger.debug("cachedValue._2")
                    Logger.debug(cachedValue._2)

                    try {
                      val cachedModelValue = partialSavePath.map(p => {
                        var v = cachedValue._2
                        p._2.foreach(pathPath => {
                          Logger.debug("In path: " + pathPath._1)
                          v \ pathPath._1 match {
                            case JArray(arr) if pathPath._2.nonEmpty =>
                              val jvlink_id = Helpers.tryo{JInt(pathPath._2.get.toInt)}.getOrElse(JString(pathPath._2.get))
                              arr.find(_ \ "jvlink_id" == jvlink_id) match {
                                case Some(o) => v = o
                                case _ =>
                                  throw new Exception("Не корректный путь: %s".format(d \ "__partialSave" match {case JString(s) => s}))
                              }
                            case o: JObject if pathPath._2.isEmpty =>
                              v = o
                            case _ =>
                              Logger.debug("Не корректный путь 3")
                              throw new Exception("Не корректный путь %s".format(d \ "__partialSave" match {case JString(s) => s}))
                          }
                        })

                        JObject(JField(f.name, v.findValue(f.name)))
                      }).getOrElse(cachedValue._2)
                      Logger.debug(cachedModelValue)

                      val cleanedInputModel = partialSavePath.map(_ => {
                        cleanJV(JObject(JField(f.name, f.value))).asInstanceOf[JObject]
                      }).getOrElse(cleanJV(f.value).asInstanceOf[JObject])

                      val nodeUuid = partialSavePath.map(_._1._2).orElse {
                        cleanedInputModel \ "uuid" match {
                          case JString(nodeUuid) => Some(nodeUuid)
                          case _ => None
                        }
                      }
                      Logger.debug("nodeUuid: " + nodeUuid)
                      nodeUuid match {
                        case Some(nodeUuid) =>
                          serverStructure.applying_model += recordName -> (partialSavePath, cleanedInputModel)
                          Logger.debug("cleanJV")
                          Logger.debug(cleanedInputModel)
                          val diffResult = cachedModelValue.jsonpDiffArrays(cleanedInputModel)
                          Logger.debug("diffResult")
                          Logger.debug(diffResult)

                          Logger.debug("Hiddens model: ")
                          Logger.debug(serverStructure.hidden_struct)

                          if (diffResult.areEqual()) {
                            func.send(JObject(List(JField("nochange", JInt(1)))))
                            FailureBox("No changes was made!")
                          } else {
                            import scala.collection.JavaConverters._
                            Logger.debug("Deletions")
                            Logger.debug(diffResult.entriesOnlyOnLeft().asScala)

                            val struct = partialSavePath.map(p => {
                              var ms = serverStructure.model_struct(recordName)
                              Logger.debug("partial save struct")
                              Logger.debug(ms.toPrettyString)
                              Logger.debug(p._2.map(_._1))
                              p._2.foreach(pathPart => {
                                ms = ms \ pathPart._1 \ "possible_links"
                              })
                              ms
                            }).getOrElse(serverStructure.model_struct(recordName))
                            Logger.debug("struct")
                            Logger.debug(struct)
                            val checkedInput = try {
                              checkInput(cleanedInputModel,
                                struct, JNull(), partialSavePath.flatMap(pp => {
                                  val path = pp._2
                                  if (path.isEmpty) None else Some(path.map(_._1).mkString("/"))
                                }),
                                //val enum, dbenum, option, plink, loclink = Value
                                serverStructure.struct_vars.map(kv => kv._1 -> {
                                  if (kv._2._2._1 == 0) {
                                    JObject(Seq(
                                      JField("depends", kv._2._1),
                                      JField("name", JString(kv._2._2._2))
                                    ))
                                  } else kv._2._1
                                }), serverStructure.hidden_struct,
                                List(struct), Nil,
                                Some(func)
                              )
                            } catch {
                              case e: Throwable =>
                                e.printStackTrace()
                                throw e
                            }

                            Logger.debug("checkedInput")
                            Logger.debug(checkedInput.map(_.toPrettyString))

                            checkedInput.map(nodeUuid -> _)
                          }
                        case _ =>
                          FailureBox("Нет uuid для записи '" + recordName + "'")
                      }
                    } catch {
                      case e: Throwable =>
                        FailureBox(e.getMessage)
                    }
                  })

                  Some(
                    inputReady.map(inputReady => {
                      (recordName, inputReady._1, inputReady._2)
                    })
                  )
                } else
                  None
              }).toList

              Logger.debug("preparedSaveInputs")
              Logger.debug(preparedSaveInputs)

              val dataReadyForSave = preparedSaveInputs.flatMap(d => {
                d match {
                  case f: FailureBox =>
                    gotFailures = true
                    f.failureChain.foreach(f => {
                      func.failure(f.msg)
                    })
                  case _ =>
                }
                d
              })

              Logger.debug(preparedSaveInputs.size + " = " + dataReadyForSave.size)
              if (preparedSaveInputs.size == dataReadyForSave.size) {
                if (preparedSaveInputs.isEmpty)
                  func.failure("Пустые данные")
                else {
                  val sequentialOpKey = DbClient.getSequentialOperationId(serverStructure.operationIdPattern,
                    serverStructure.request_name,
                    serverStructure.model_struct.keys.toList,
                    dataReadyForSave.map(x => x._1 -> x._2))

                  Logger.debug(sequentialOpKey)

                  val frontOperationId = UUID.randomUUID().toString
                  //TODO: Создаём временный actor с ключём opId
                  frontOperations += frontOperationId -> func
                  serverStructure.frontOperations += frontOperationId
                  //по opId принимаем данные об ошибках во время сохранения
                  DbClient.save(sequentialOpKey, frontOperationId, serverStructure.request_name, dataReadyForSave.map(d => {
                    SaveEl(d._1, d._2, d._3.toString)
                  }), partialSavePathString, Nil)
                }
              } else if (gotFailures)
                func.failure("Не корректные входные данные")
            }
          }).isEmpty) {
            func.failure("Не правильный путь path, или данные о структуре устарели для текущей сессии")
          }
        })
        //Logger.debug("Saving: " + AngularRequestServerStructure.get.get(path).map(_.model_struct))
    }

    /*
    if (!done)
      func.done()
     */

    //roundTripFutureCall(data, func, askValidateChannel(true, _))
    /*
    askValidateChannel(jsonDataToStringMap(data)).onComplete{
      case Success(resp) =>
        func.send(Jckson.mapToJson(resp))

      case scala.util.Failure(e) =>
        func.failure("message send response exception")
        //Logger.debug("message send response exception")
        e.printStackTrace()
    }
     */
  }

  private def cleanJV(jv: JValue): JValue = {
    jv match {
      case obj: JObject =>
        JObject( obj.children.collect{case f: JField if f.value != JNull() &&
          f.name != "__recn" &&
          f.name != "jvlink_auto" &&
          !f.name.endsWith("jv_struct") &&
          !f.name.endsWith("jv_rights") => JField(f.name, cleanJV(f.value))}.filter(_.value match {
          case chObj: JObject => chObj.children.nonEmpty
          case _ => true
        }) )
      case arr: JArray =>
        JArray(arr.arr.map(arrObj => {
          val cleanedArrObj = cleanJV(arrObj)
          val cleanThisObj = cleanedArrObj match {
            case o: JObject => !o.children.exists(ch => {
              val chField = ch
              chField.name != "jvrel_prop" &&
                chField.name != "jvlink_id" &&
                chField.name != "jvlink_auto" &&
                (!chField.value.isInstanceOf[JArray] || chField.value.asInstanceOf[JArray].arr.nonEmpty)
            })
            case _ => false
          }
          cleanThisObj -> cleanedArrObj
        }).filter(!_._1).map(_._2).toList)
      case x => x
    }
  }

  private def expandTree(en_o: JValue): List[BigInt] = {
    en_o\"id" match {
      case JInt(num) => List(num)
      case null =>
        en_o\"leaf" match {
          case JArray(arr) =>
            arr.flatMap(expandTree(_)).toList
          case _ => en_o\"opts" match {
            case JArray(arr) =>
              arr.map(en_o => en_o\"id" match {case JInt(i) => i}).toList
            case _ => Nil
          }
        }
      case _ => Nil
    }
  }


  def prepareInput(recordName: String, jv: JObject, structure: AngularServerStructure,
                   partialSavePathString: Option[String] = None): Box[JValue] = {
    val partialSavePath = partialSavePathString match {
      case Some(s) =>
        val path = s.split("""\.""")
        val h = path.head.split('#')
        if (h.length == 2 && h(1).nonEmpty) {
          Some(
            (h(0) -> h(1)) -> path.tail.toSeq.map(p => {
              val splits = p.split("#")
              if (splits.size == 2 && splits(1).nonEmpty) {
                splits(0).trim -> Some(splits(1).trim)
              } else if (splits.size <= 2) {
                splits(0).trim -> None
              } else
                throw new Exception("Не корректный путь")
            })
          )
        } else
          throw new Exception("Не корректный путь")
      case _ =>
        None
    }

    val cleanedInputModel = cleanJV(jv).asInstanceOf[JObject]
    val struct = partialSavePath.map(p => {
      var ms = structure.model_struct(recordName)
      Logger.debug("partial save struct")
      Logger.debug(ms.toPrettyString)
      Logger.debug(p._2.map(_._1))
      p._2.foreach(pathPart => {
        ms = ms \ pathPart._1 \ "possible_links"
      })
      ms
    }).getOrElse(structure.model_struct(recordName))
    Logger.debug(cleanedInputModel.toPrettyString)
    Logger.debug("struct")
    Logger.debug(struct.toPrettyString)
    checkInput(cleanedInputModel,
      struct, JNull(), partialSavePath.flatMap(pp => {
        val path = pp._2
        if (path.isEmpty) None else Some(path.map(_._1).mkString("/"))
      }),
      structure.struct_vars.map(kv => kv._1 -> {
        if (kv._2._2._1 == 0) {
          JObject(Seq(
            JField("depends", kv._2._1),
            JField("name", JString(kv._2._2._2))
          ))
        } else kv._2._1
      }), structure.hidden_struct,
      List(struct), Nil,
      None
    )
  }

  private val varExtractRegexp = "avp(\\d+)".r
  private def getVariableByName(varName: String, init_vars: Map[String, JValue]): Option[JValue] = {
    Logger.debug("Get variable by name: " + varName)
    varName match {
      case varExtractRegexp(i) =>
        init_vars.get(i)
      case _ =>
        init_vars.get(varName)
    }
  }
  private def checkInput(in: JValue, struct: JValue, rel_prop_filter: JValue, parentPath: Option[String],
                         init_vars: Map[String, JValue],
                         hidden_struct: Map[String, JValue], rec_struct: List[JValue], deletions: Seq[JField],
                         func: Option[RoundTripHandlerFunc]): Box[JValue] = {
    Logger.debug("CHECK INPUT")
    in match {
      case in: JObject =>
        val deleted_parametric_fields = deletions.flatMap(del_field => {
          if (!in.has(del_field.name) && (struct\del_field.name).has("available_p")) {
            Some(JField(del_field.name, JObject(List(
              JField("parametric", JBool(true)),
              JField("links", JObject(List()))
            ))))
          } else None
        })
        val filtered_fields = struct\"_filter_fields_"
        val rel_prop_f_fields = rel_prop_filter.children.collect{case f: JField => f}
        var already_filtered: List[String] = Nil

        val new_flds = in.children.map {
          case f: JField =>
            struct \ f.name match {
              case obj_s: JObject =>
                val obj = obj_s\"rec" match {
                  case JInt(i) => rec_struct(i.toInt)
                  case _ => obj_s
                }

                val is_file = obj\"jvfile_field" match {case JBool(true) => true case _ => false}
                val is_date = obj\"jvdate_field" match {case JBool(true) => true case _ => false}
                val is_enum = obj\"jvenum_field" match {case JBool(true) => true case _ => false}
                val is_opt_enum = obj\"jvopt_enum" match {case JBool(true) => true case _ => false}
                val is_opt = obj\"jvopt_field" match {case JBool(true) => true case _ => false}
                val relp_unique = obj\"rel_prop_unique" match {case JArray(arr) => arr case _ => Nil}
                val distinct_merge = obj\"distinct_merge" match {case JBool(true) => true case _ => false}

                if (f.name == "jvrel_prop") {
                  //TODO: filter relationship property or add if not set
                  var already_filtered_rel_p: List[String] = Nil
                  val filt_ok = f.value.children.map {
                    case f: JField => rel_prop_f_fields.find(filt_f => {
                      if (filt_f.name == f.name) {
                        already_filtered_rel_p ::= filt_f.name
                        true
                      } else false
                    }).forall(_.value == f.value)
                    case _ => false
                  }.forall(x => x)

                  if (filt_ok) {
                    Full(JField(f.name, JObject(f.value.children.collect{case f: JField => f} ++
                      rel_prop_f_fields.collect{case f: JField if !already_filtered_rel_p.contains(f.name) =>
                        f
                      }
                    )))
                  } else {
                    func.foreach(_.failure("bad rel_prop (does not match filter): " + (f.name -> f.value)))
                    FailureBox("Relationship property does not match filter")
                  }
                } else if (is_file) {
                  Empty/*
                  Full(JField(f.name, JObject(
                    (f.value\"upload_id" match {case JString(upl_id) => {
                      val res = AngularUploads.get(S.renderVersion, upl_id)
                      res.foreach(upl += upl_id -> _)
                      res.map(_ => List( JField("upload_id", JString(upl_id)) ))
                    } case _ => None}).getOrElse(Nil) :::
                      JField("fname", f.value\"fname") ::
                      JField("is_image", f.value\"is_image") ::
                      JField("jvfile_field", f.value\"jvfile_field") :: Nil)
                  ))
                  */
                } else if (is_date) {
                  Full(JField(f.name, JObject(
                    JField("dval", f.value\"dval") ::
                      JField("date_format", f.value\"date_format") ::
                      JField("jvdate_field", JBool(true)) :: Nil)
                  ))
                } else if (is_enum) {
                  var isDbEnum = false
                  val possible_enums = (obj\"enm" match {
                    case JString(s) =>
                      val variables = getVariableByName(s, init_vars)
                      println("TRY FIND ENUM: " + s)
                      println(variables)
                      variables.map {
                        case jarr: JArray => jarr.arr.map(en_o => en_o \ "id" match {case JInt(i) => i}).toList
                        case jobj: JObject =>
                          isDbEnum = true
                          /*
                          val enm = DBEnums.getEnumByNameId((jobj\"name").asInstanceOf[JString].s)
                          val poss = enm.map(e => e.values().map(v => BigInt(v._1)) :+ BigInt(-1L)).getOrElse(Nil)
                          poss :+ (poss.max + 1)
                           */
                          Nil
                      }.getOrElse(Nil)
                    case _ => Nil
                  }) :+ BigInt(-1)
                  val min_poss = possible_enums.min
                  Logger.debug("possible_enums")
                  Logger.debug(possible_enums)
                  val numeric_enum = obj\"jvenum_field_numeric" match {case JBool(true) => true case _ => false}
                  val e_val = if (numeric_enum) f.value else f.value\"e_val"
                  //println(obj\"enm")
                  //println("GETTING e_val: " + numeric_enum + " " + f.value + "   e_val: " + e_val + " possibles: " + possible_enums)
                  if (is_opt_enum && e_val == JInt(-1)) Full(JField(f.name, JNull()))
                  else {
                    if (isDbEnum) {
                      //TODO: check is in db enum by elastic search
                      Full(JField(f.name, JObject(
                        JField("jvenum_field", JBool(true)) :: Nil
                      ) ))
                    } else
                    e_val match {
                      case JInt(num) if possible_enums.contains(num) || num == 0 =>
                        val good_num = if (num < min_poss) min_poss else num
                        (filtered_fields\f.name match {
                          case JInt(f_num) =>
                            already_filtered ::= f.name
                            if (f_num == good_num) Some(good_num)
                            else None
                          case _ => Some(num)
                        }) match {
                          case Some(filtered_num) =>
                            if (numeric_enum) Full(JField(f.name, JInt(filtered_num)))
                            else
                              Full(JField(f.name, JObject(
                                JField("e_val", JInt(filtered_num)) ::
                                  JField("jvenum_field", JBool(true)) :: Nil
                              )
                              ))
                          case _ =>
                            func.foreach(_.failure("bad field (enum val does not match filter!!): " + (f.name -> f.value)))
                            FailureBox("Enum val does not match filter")
                        }
                      case JArray(arr) =>
                        arr.find {
                          case JInt(num) if possible_enums.contains(num) => false
                          case _ => true
                        } match {
                          case None =>
                            if (numeric_enum) Full(JField(f.name, JArray(arr.toList)))
                            else
                              Full(JField(f.name, JObject(
                                JField("e_val", JArray(arr.toList)) ::
                                  JField("jvenum_field", JBool(true)) :: Nil
                              )
                              ))
                          case _ =>
                            func.foreach(_.failure("bad field (not in possible enums!!): " + (f.name -> f.value)))
                            FailureBox("Field not in possible enums: " + (f.name -> e_val))
                        }
                      case _: JNull | null/* if (isDbEnum)*/ =>
                        Full(JField(f.name, JObject(
                          JField("jvenum_field", JBool(true)) :: Nil
                        ) ))
                      case _ =>
                        func.foreach(_.failure("bad field (not in possible enums): " + (f.name -> f.value)))
                        FailureBox("Field not in possible enums: " + (f.name -> e_val) + "   " + possible_enums)
                    }
                  }
                } else if (is_opt) {
                  var maxOpt: Option[BigInt] = None
                  Logger.debug("Check possible options: " + obj\"opt")
                  val possible_opts = (obj\"opt" match {
                    case JString(s) => getVariableByName(s, init_vars).map(_.asInstanceOf[JArray].arr.toList.flatMap(en_o => {
                      val l = expandTree(en_o)
                      l.foreach(i => if (maxOpt.forall(i > _)) maxOpt = Some(i))
                      l
                    })).getOrElse(Nil)
                    case _ => Nil
                  }) ::: maxOpt.map(_ + 1).toList
                  val sngl = obj\"sngl" match {
                    case JBool(b) => b
                    case _ => false
                  }

                  f.value\"o_val" match {
                    case JArray(opt_arr) if opt_arr.filter(_ match {
                      case null => false
                      case _ => true
                    }).map{
                      case JInt(opt) => opt < 0 || possible_opts.contains(opt)
                      case _ => false
                    }.forall(x => x) =>
                      val distinct = opt_arr.toList.distinct.sortWith(_.intValue() < _.intValue())
                      Full(JField(f.name, JObject(
                          JField("o_val", JArray(if (sngl) distinct.headOption.toList else distinct)) ::
                            JField("jvopt_field", JBool(true)) :: Nil
                        ))
                      )
                    case _ =>
                      func.foreach(_.failure("bad field (not in possible options): " + (f.name -> f.value)))
                      FailureBox("Field not in possible options: " + f.name + " : " + possible_opts)
                  }
                } else {
                  //TODO: links
                  //LINKS

                  val single = obj\"single" match {case JBool(true) => true case _ => false}
                  val hidden = obj\"hidden" match {case JBool(true) => true case _ => false}
                  val (parametric, parameters) = obj\"available_p" match {
                    case JString(s) =>
                      !s.isEmpty -> getVariableByName(s, init_vars).map(
                        _.asInstanceOf[JArray].arr.toList.map(_.asInstanceOf[JString].textValue())).getOrElse(Nil)
                    case _ => false -> Nil}

                  val fieldKey = parentPath.map("%s/%s".format(_, f.name)).getOrElse(f.name)
                  val hStr = if (hidden) hidden_struct.get(fieldKey) else None
                  Logger.debug("TESTING HIDDEN LINK: " + hidden + " " + f.name + " " + fieldKey + " : " + hidden_struct)
                  val link_field = if (hidden && (hStr.isEmpty ||
                    {
                      Logger.debug("Check hidden record")
                      Logger.debug(hStr.map(_.toPrettyString))

                      /*
                      hStr.link_rules._2.map(hStr_in => {
                        //println("CHECK HIDDEN LINK STRUCTURE")
                        if (hStr_in.to.length >= hStr_in.from.length) {
                          //println("CHECK HIDDEN LINK STRUCTURE CASE 1")
                          func.foreach(_.failure("bad field (no hidden link to is longer than hidden link from): " + (f.name -> f.value)))
                          true
                        } else if (hStr_in.to.exists(_.repeat) || hStr_in.from.exists(_.repeat)) {
                          //println("CHECK HIDDEN LINK STRUCTURE CASE 2")
                          func.foreach(_.failure("bad field (hidden link from or to can not contain repeats (*)): " + (f.name -> f.value)))
                          true
                        } else if ({var i = 0; !hStr_in.to.map(to => {
                          val from = hStr_in.from(i)
                          i += 1
                          to.dir == from.dir && to.ref_type == from.ref_type
                        }).foldLeft(true)(_ && _)}) {
                          //println("CHECK HIDDEN LINK STRUCTURE CASE 3")
                          func.foreach(_.failure("bad field (hidden link from doesn't match to): " + (f.name -> f.value)))
                          true
                        } else if (hStr.uselink && !hStr_in.to.isEmpty) {
                          //println("CHECK HIDDEN LINK STRUCTURE CASE 4")
                          func.foreach(_.failure("bad field (hidden link should have empty to if useslink): " + (f.name -> f.value)))
                          true
                        } else false
                      }).foldLeft(false)(_ || _)
                      */
                      false
                    })) {
                    func.foreach(_.failure("bad field (wrong hidden link structure at server): " + (f.name -> f.value)))
                    FailureBox("Incorrect hidden link definition")
                  } else if (single || parametric) {
                    Logger.debug("AngularSnippet debug, single or parametric: " + f.name)
                    f.value match {
                      case o: JObject =>
                        {
                          if (parametric) {
                            val inner_del = deletions.find(_.name == f.name).map(_.value.children.collect{case f: JField => f}).getOrElse(Nil)
                            val new_flds = o.children.collect{case jf: JField if jf.name != "jv_struct" => jf}.map(o => {
                              checkInput(o.value, obj\"possible_links", obj\"rel_props_filter", Some(fieldKey),
                                init_vars, hidden_struct, obj :: rec_struct,
                                inner_del.find(_.name == o.name).map(_.value.children.collect{case f: JField => f}).getOrElse(Nil),
                                func).map(v => JField(o.name, v))
                            })
                            new_flds.find { case _: FailureBox => true case _ => false } match {
                              case Some(fl) => fl.asInstanceOf[FailureBox]
                              case _ => Full(new_flds.flatten)
                            }
                          } else {
                            val new_obj = JObject( o.children.collect{case jf: JField if (jf.name != "jv_struct") => jf} )
                            val del = deletions.find(_.name == f.name).map(_.value.children.collect{case f: JField => f}).getOrElse(Nil)
                            val possibles = obj\"possible_links"

                            Logger.debug("SINGLE LINK AngularSnippet: " + (del.size, (obj\"possible_links").children.size))

                            Logger.debug("SINGLE LINK: " + del)
                            //Logger.debug("SINGLE LINK: " + getJStructure(JObject(del), false) + " " + getJStructure(obj, true))

                            /*
                            val (new_del, single_del) = if (del.size == (obj\"possible_links").children.size &&
                              getJStructure(JObject(del), false) == getJStructure(obj, true)) {
                              Logger.debug("ADDING DEL: " + possibles.children + " " + del + " " + deletions)
                              //println("ADDING DEL: " + possibles.children + " " + del + " " + deletions)
                              Nil -> true
                            } else del -> false
                             */
                            val (new_del, single_del) = del -> false
                            checkInput(new_obj, possibles, obj\"rel_props_filter", Some(fieldKey),
                                init_vars, hidden_struct, obj :: rec_struct,
                                new_del, func)
                              .map(v => v.children.collect{case f: JField => f} ++ {if (single_del) {
                                JField("jv_del", JBool(true)) :: Nil
                              } else Nil})
                          }
                        }.map(l => JField("links", JObject(l)))
                      case _ =>
                        func.foreach(_.failure("bad field (should be object): " + (f.name -> f.value)))
                        FailureBox("Field should be an object")
                    }
                  } else {
                    f.value match {
                      case JArray(arr) =>
                        val new_arr = arr.map {
                          case o: JObject =>
                            //recursion
                            val link_flds = checkInput(o, obj \ "possible_links", obj \ "rel_props_filter", Some(fieldKey),
                              init_vars, hidden_struct, obj :: rec_struct,
                              deletions.find(_.name == f.name).map(_.value.children.collect { case f: JField => f }).getOrElse(Nil),
                              func)

                            if (hidden) {
                              hStr.map(hiddenStr => {
                                Logger.debug("Got hidden structure")
                                Logger.debug(hiddenStr.toPrettyString)
                                val hiddenRecordKey = hiddenStr \ "key_field" match {case JString(s) => s}
                                val new_link_flds = link_flds.map(link_flds => {
                                  val key_exisits = link_flds.has(hiddenRecordKey)
                                  if (!key_exisits) {
                                    val auto_key = obj \ "possible_links" \ hiddenRecordKey
                                    JObject(link_flds.children :+ JField(hiddenRecordKey, auto_key))
                                  } else link_flds
                                })

                                new_link_flds
                              }).getOrElse(link_flds)
                            } else link_flds
                          case _ =>
                            func.foreach(_.failure("bad element at array (should be array): " + (f.name -> f.value)))
                            FailureBox("Bad element at array")
                        }
                        (new_arr.find { case _: FailureBox => true case _ => false } match {
                          case Some(fl) => fl.asInstanceOf[FailureBox]
                          case _ => Full(new_arr.toList.flatten)
                        }).map(l => JField("links", JArray(l)))
                      case _ =>
                        func.foreach(_.failure("bad field (should be array): " + (f.name -> f.value)))
                        FailureBox("Field should be array")
                    }
                  }

                  link_field.map(fld => {
                    JField(f.name, JObject(
                      (if (relp_unique.nonEmpty) {
                        JField("rel_prop_unique", JArray(relp_unique.toList)) :: Nil
                      } else Nil) :::
                        (if (distinct_merge) {
                          JField("distinct_merge", JBool(distinct_merge)) :: Nil
                        } else Nil) :::
                        (if (single) {
                          JField("single", JBool(true)) :: Nil
                        } else Nil) :::
                        (if (hidden) {
                          val fieldKey = parentPath.map("%s/%s".format(_, f.name)).getOrElse(f.name)
                          val hiddenStr = hidden_struct(fieldKey)
                          //println("SEND HIDDEN PARAMETERS", f.name, hiddenStr)
                          Logger.debug("SEND HIDDEN PARAMETERS")
                          Logger.debug(hiddenStr.toPrettyString)
                          JField("hidden", JBool(true)) :: JField("hiddenStr", hiddenStr) :: Nil
                          /*
                            JField("hidden_from_to", JArray(hiddenStr.link_rules._2.map(link_rule => {
                              JObject(
                                JField("hidden_from", JArray(link_rule.from.map(fr => {
                                  JObject(JField("rtype", JString(fr.ref_type)) ::
                                    JField("dir", JString(fr.dir.toString)) ::
                                    JField("repeat", JBool(fr.repeat)) :: Nil)
                                }))) ::
                                  JField("hidden_to", JArray(link_rule.to.map(fr => {
                                    JObject(JField("rtype", JString(fr.ref_type)) ::
                                      JField("dir", JString(fr.dir.toString)) ::
                                      JField("repeat", JBool(fr.repeat)) :: Nil)
                                  }))) ::
                                  JField("is_main", JBool(link_rule.main)) ::
                                  JField("sortable", JBool(link_rule.sortable)) ::
                                  (link_rule.other match {
                                    case Some(oth) => JField("other",oth) :: Nil
                                    case _ => Nil
                                  })
                              )
                            }))) ::
                            JField("hidden_from_rec", JString(hiddenStr.link_rules._1)) ::
                            hiddenStr.require.map(req => {
                              JField("hidden_require", JObject(
                                JField("links", JArray(req.link.map(rl => {
                                  JObject(JField("rtype", JString(rl.ref_type)) ::
                                    JField("dir", JString(rl.dir.toString)) ::
                                    JField("repeat", JBool(rl.repeat)) :: Nil)
                                }))) ::
                                  JField("tag", JString(req.rec_tag)) ::
                                  req.field.map(field => JField("field", JString(field)) :: Nil).getOrElse(Nil)
                              )) :: Nil
                            }).getOrElse(Nil) :::
                            JField("hidden_key_field", hiddenStr \ "key_field") ::
                            JField("hidden_can_create", hiddenStr \ "create") ::
                            JField("hidden_pick", JBool(hiddenStr.pick)) ::
                            JField("hidden_editable", JBool(hiddenStr.editable)) ::
                            JField("hidden_uselink", JBool(hiddenStr.uselink)) ::
                            JField("hidden_delete", JBool(hiddenStr.delete)) :: Nil
                           */
                        } else Nil) :::
                        (if (parametric) JField("parametric", JBool(true)) :: Nil else Nil) ::: fld :: Nil
                    ))
                  })
                }
              case null =>
                (if (f.name.endsWith("_jv_struct")) {
                  true -> struct \ f.name.substring(0, f.name.length - 10)
                } else if (f.name.endsWith("_jv_del")) {
                  false -> struct \ f.name.substring(0, f.name.length - 7)
                } else if (f.name.endsWith("_jv_rights")) {
                  true -> struct \ f.name.substring(0, f.name.length - 10)
                } else false -> JNull()) match {
                  case (is_empty: Boolean, obj_s: JObject) =>
                    if (is_empty) Empty
                    else {
                      val obj = obj_s \ "rec" match {
                        case JInt(i) => rec_struct(i.toInt)
                        case _ => obj_s
                      }
                      val single = obj \ "single" match {
                        case JBool(true) => true
                        case _ => false
                      }
                      val hidden = obj \ "hidden" match {
                        case JBool(true) => true
                        case _ => false
                      }

                      if (!single) {
                        Full(JField(f.name, f.value))
                      }
                      else {
                        func.foreach(_.failure("bad field1: " + (f.name -> f.value)))
                        FailureBox("Bad field")
                      }
                    }
                  case _ =>
                    if (f.value.isInstanceOf[JArray] || f.value.isInstanceOf[JObject]) {
                      /*
                      func.foreach(_.failure("bad field2: " + (f.name -> f.value)))
                      Failure("Bad field")
                      *
                      */
                      Empty
                    }
                    else Full(f)
                }
              case _ =>
                filtered_fields\f.name match {
                  case null => Full(f)
                  case x =>
                    Logger.debug("Check filtered fields: " + x)
                    if (x == f.value) {
                      already_filtered ::= f.name
                      Full(JField(f.name, x))
                    } else if (x match {case JArray(arr) => arr.exists(_ == JObject(JField(f.name, f.value))); case _ => false}) {
                      already_filtered ::= f.name
                      Full(JField(f.name, f.value))
                    } else {
                      func.foreach(_.failure("bad field (does not match filter): " + (f.name -> f.value)))
                      FailureBox("Field does not match filter " + (f.name -> f.value))
                    }
                }
            }
          case _ => Empty
        } ++ deleted_parametric_fields.map(Full(_))

        val finally_fields = new_flds ++
          filtered_fields.children.collect{case f: JField if !already_filtered.contains(f.name) =>
            Full(f)
          }

        (finally_fields.find { case fl: FailureBox => true case _ => false } match {
          case Some(fl) => fl.asInstanceOf[FailureBox]
          case _ => Full(finally_fields.flatten)
        }).map(JObject(_))

      case x => Full(x)
    }
  }


  import scala.collection.JavaConverters._
  private[dataadapter] def clearFrontOperations(opIds: Seq[String]): Unit = {
    opIds.foreach(frontOperations -= _)
  }
  def updateFrontOp(frontOperationId: String, jv: String, done: Boolean): Boolean = {
    Logger.debug("updateFrontOp: " + done)
    Logger.debug(frontOperationId)
    Logger.debug(jv)
    frontOperations.get(frontOperationId).exists(func => {
      Logger.debug("updateFrontOp jv: " + jv)
      if (jv.nonEmpty)
        func.sendJVal(jv)
      if (done)
        func.done()
      true
    })
  }
  def failFrontOp(frontOperationId: String, message: String): Boolean = {
    frontOperations.get(frontOperationId).exists(func => {
      Logger.debug("failFrontOp jv: " + message)
      func.failure(message)
      true
    })
  }
  private val frontOperations: concurrent.Map[String, RoundTripHandlerFunc] = new ConcurrentHashMap[String, RoundTripHandlerFunc]().asScala
}
