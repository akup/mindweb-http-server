package net.aklabs.helpers

import java.io.{File, InputStream}
import java.lang.reflect.{ParameterizedType, Type}
import java.util
import collection.JavaConverters._
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node._
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.google.common.collect.{MapDifference, Maps}
import org.pmw.tinylog.Logger

import scala.language.implicitConversions



class RichJObj(val obj: ObjectNode) {
  def children: Seq[JsonHelpers.JField] = {
    obj.fields().asScala.toSeq.map(e => JsonHelpers.JField(e.getKey, e.getValue))
  }
  def fields: Seq[JsonHelpers.JField] = children
  def +(jf: JsonHelpers.JField): JsonNode = {
    JsonHelpers.JObject(
      obj.fields().asScala.toSeq.map(e => e.getKey -> e.getValue)
    ).set(jf.name, jf.value)
  }
  def +=(jf: JsonHelpers.JField): JsonNode = {
    obj.set(jf.name, jf.value)
  }
  def ++(sjf: Seq[JsonHelpers.JField]) : JsonNode = {
    val copy = JsonHelpers.JObject(
      obj.fields().asScala.toSeq.map(e => e.getKey -> e.getValue)
    )
    sjf.foreach(jf => copy.set(jf.name, jf.value))
    copy
  }
  def ++=(sjf: Seq[JsonHelpers.JField]) : JsonNode = {
    sjf.foreach(jf => obj.set(jf.name, jf.value))
    obj
  }

  def diff(otherObj: JsonHelpers.JObject): MapDifference[String, AnyRef] = {
    val leftMap = JsonHelpers.Jckson.clearMap(
      JsonHelpers.Jckson.mapper.convertValue(obj, classOf[java.util.HashMap[String, Object]])
    )
    val rightMap = JsonHelpers.Jckson.clearMap(
      JsonHelpers.Jckson.mapper.convertValue(otherObj, classOf[java.util.HashMap[String, Object]])
    )

    Maps.difference(leftMap, rightMap)
  }
  def jsonpDiff(otherObj: JsonHelpers.JObject,
                arrayKeys: Seq[String] = Nil): MapDifference[String, Object] = {
    Logger.debug("jsonpDiff")
    val leftMap = JsonHelpers.Jckson.clearFlatMap(
      JsonHelpers.Jckson.mapper.convertValue(obj, classOf[java.util.HashMap[String, Object]]),
      arrayKeys = arrayKeys
    )
    val rightMap = JsonHelpers.Jckson.clearFlatMap(
      JsonHelpers.Jckson.mapper.convertValue(otherObj, classOf[java.util.HashMap[String, Object]]),
      arrayKeys = arrayKeys
    )

    Logger.debug("jsonpDiff")
    Logger.debug("left:")
    Logger.debug(leftMap)
    Logger.debug("right:")
    Logger.debug(rightMap)

    Maps.difference(leftMap, rightMap)
  }
  def jsonpDiffArrays(obj: JsonHelpers.JObject): MapDifference[String, Object] =
    jsonpDiff(obj, Seq("jvlink_id" /*, "jvsort_id"*/))
}
class RichJNode(val jv: JsonNode) {
  def \(name: String): JsonNode = {
    if (jv == null) null
    else jv.get(name) match {
      case i: IntNode => JsonNodeFactory.instance.numberNode(i.bigIntegerValue()).asInstanceOf[BigIntegerNode]
      case i: LongNode => JsonNodeFactory.instance.numberNode(i.bigIntegerValue()).asInstanceOf[BigIntegerNode]
      case i: ShortNode => JsonNodeFactory.instance.numberNode(i.bigIntegerValue()).asInstanceOf[BigIntegerNode]
      case i: DecimalNode => JsonNodeFactory.instance.numberNode(i.bigIntegerValue()).asInstanceOf[BigIntegerNode]
      case x => x
    }
  }
  def toOpt: Option[JsonNode] = jv match {
    case _: MissingNode => None
    case _: NullNode => None
    case json     => Some(json)
  }
  def children: Seq[JsonHelpers.JField] = jv match {
    case obj: ObjectNode => obj.fields().asScala.toSeq.map(e => JsonHelpers.JField(e.getKey, e.getValue))
    case _ => Nil
  }
  def fields: Seq[JsonHelpers.JField] = children

  def +(jf: JsonHelpers.JField): JsonNode = jv match {
    case thisObj: ObjectNode => new RichJObj(thisObj) + jf
    case x => x
  }
  def +=(jf: JsonHelpers.JField): JsonNode = jv match {
    case thisObj: ObjectNode => new RichJObj(thisObj) += jf
    case x => x
  }
  def ++(sjf: Seq[JsonHelpers.JField]) : JsonNode = jv match {
    case thisObj: ObjectNode => new RichJObj(thisObj) ++ sjf
    case x => x
  }
  def ++=(sjf: Seq[JsonHelpers.JField]) : JsonNode = jv match {
    case thisObj: ObjectNode => new RichJObj(thisObj) ++= sjf
    case x => x
  }

  def diff(otherObj: JsonHelpers.JObject): MapDifference[String, AnyRef] = jv match {
    case thisObj: ObjectNode => new RichJObj(thisObj).diff(otherObj)
    case _ => throw new Exception("Can diff only objects")
  }
  def jsonpDiff(otherObj: JsonHelpers.JObject,
                arrayKeys: Seq[String] = Nil): MapDifference[String, Object] = jv match {
    case thisObj: ObjectNode => new RichJObj(thisObj).jsonpDiff(otherObj, arrayKeys)
    case _ => throw new Exception("Can diff only objects")
  }
  def jsonpDiffArrays(obj: JsonHelpers.JObject): MapDifference[String, Object] =
    jsonpDiff(obj, Seq("jvlink_id" /*, "jvsort_id"*/))
}
class RichJArray(val array: ArrayNode) {
  def arr: Iterable[JsonNode] = array.elements().asScala.toIterable
}
object JsonHelpers {
  type JValue = JsonNode
  type JMissing = MissingNode
  type JNull = NullNode
  type JString = TextNode
  type JInt = BigIntegerNode
  type JDouble = DoubleNode
  type JBool = BooleanNode
  type JObject = ObjectNode
  type JArray = ArrayNode

  implicit def s2JValue(s: String): JValue = JString(s)
  implicit def i2JValue(i: Int): JValue = JInt(i)
  implicit def bi2JValue(i: BigInt): JValue = JInt(i)
  implicit def l2JValue(i: Long): JValue = JInt(i)
  implicit def f2JValue(d: Float): JValue = JDouble(d)
  implicit def d2JValue(d: Double): JValue = JDouble(d)
  implicit def bd2JValue(i: BigDecimal): JValue = JDouble(i)
  implicit def b2JValue(i: Boolean): JValue = JBool(i)
  implicit def seq2JValue[A](s: Seq[A])(implicit c: A => JValue): JArray = JArray(s.map(c))
  implicit def a2JValue[A](s: Array[A])(implicit c: A => JValue): JArray = JArray(s.map(c))
  implicit def m2JValue[A](m: Map[String, A])(implicit c: A => JValue): JObject =
    JObject(m.map(e => e._1 -> c(e._2)))
  implicit def ts2JValue[A](m: Seq[(String, A)])(implicit c: A => JValue): JObject =
    JObject(m.map(e => e._1 -> c(e._2)))
  implicit def any2JValue(v: Any): JValue = any2JValue(v, error_v = "v")
  def any2JValue(v: Any, error_v: String): JValue = v match {
    case s: String => JString(s)
    case i: Int => JInt(i)
    case i: BigInt => JInt(i)
    case i: Long => JInt(i)
    case i: Float => JDouble(i)
    case i: Double => JDouble(i)
    case i: BigDecimal => JDouble(i.toDouble)
    case b: Boolean => JBool(b)
    case jv: JValue => jv
    case s: Seq[_] => JArray(s.map(any2JValue(_, error_v)))
    case a: Array[_] => JArray(a.map(any2JValue(_, error_v)))
    case m: Map[String, _] => JObject(m.toSeq.map(kv => JField(kv._1, any2JValue(kv._2, error_v))))
    case null => JNull()
    case x => throw new Exception("Invalid %s %s".format(error_v, x.getClass.toString))
  }

  implicit def anyseq2jvseq[JV](fv: Seq[JV])(implicit fn: JV => JValue): Seq[JValue] = fv.map(fn)

  implicit def field2tuple(field: JField): (String, JValue) = field.name -> field.value
  implicit def tuple2field[JV](fv: (String, JV))(implicit fn: JV => JValue): JField = JField(fv._1, fv._2)
  //implicit def fieldseq2tupleseq(fields: Seq[JField]): Seq[(String, JValue)] = fields.map(field2tuple)
  implicit def tupleseq2fieldseq[JV](fv: Seq[(String, JV)])(implicit fn: JV => JValue): Seq[JField] = fv.map(tuple2field)
  //implicit def anytupleseq2field(fv: Seq[(String, Any)]): Seq[JField] = fv.map(anytuple2field)
  implicit def node2rich(jv: JValue): RichJNode = new RichJNode(jv)
  implicit def obj2rich(o: JObject): RichJObj = new RichJObj(o)
  implicit def rich2node(rich: RichJNode): JValue = rich.jv
  implicit def array2rich(array: JArray): RichJArray = new RichJArray(array)
  implicit def rich2array(rich: RichJArray): JArray = rich.array
  /*
  implicit def fieldseq2object[JF](fields: Seq[JF])(implicit fn: JF => (String, JValue)): JObject =
    JObject(fields.map(fn))
   */
  implicit def fieldmap2object[JV](fields: Map[String, JV])(implicit fn: JV => JValue): JObject =
    JObject(fields.map(kv => kv._1 -> fn(kv._2)))

  //How to use
  /*
  val x: Seq[(String, JValue)] = Seq(JField("x", 0), JField("y", "hello"))
  val o: JObject = Map("x" -> 0, "y" -> "hello")
  val o2: JObject = Seq("x" -> 0, "y" -> "hello")
  val seq: Seq[JField] = Seq("x" -> 0, "y" -> "hello")
  val arr: JArray = JArray(Seq("x" -> 0, "y" -> "hello"))
   */

  case class JField(name: String, value: JValue) {
    def toTuple: (String, JValue) = name -> value
  }

  implicit def field2option(field: JField): Option[JField] = Some(field)
  implicit def tuple2option[JV](fv: (String, JV))(implicit fn: JV => JValue): Option[JField] =
    Some(JField(fv._1, fv._2))
  implicit def optiontuple2field[JV](fv: Option[(String, JV)])(implicit fn: JV => JValue): Option[JField] =
    fv.map(fv => JField(fv._1, fv._2))

  object JObject {
    def apply(name: String, value: JValue): JObject = {
      val n = JsonNodeFactory.instance.objectNode()
      n.set(name, value)
      n
    }
    def apply(): JObject = JsonNodeFactory.instance.objectNode()
    def apply(fields: Seq[JField]): JObject = {
      val n = JsonNodeFactory.instance.objectNode()
      fields.foreach(f => n.set(f._1, f._2): ObjectNode)
      n
      //JsonNodeFactory.instance.objectNode().setAll(fields.map(jf => jf.name -> jf.value).toMap.asJava)
    }
    def apply(fields: JField*)(implicit dummy: DummyImplicit): JObject = {
      val n = JsonNodeFactory.instance.objectNode()
      fields.foreach(f => n.set(f._1, f._2): ObjectNode)
      n
      //JsonNodeFactory.instance.objectNode().setAll(fields.map(f => f.name -> f.value).toMap.asJava)
    }
    def apply(fields: Map[String, JValue]): JObject = {
      val n = JsonNodeFactory.instance.objectNode()
      fields.foreach(f => n.set(f._1, f._2): ObjectNode)
      n
      //JsonNodeFactory.instance.objectNode().setAll(fields.asJava)
    }
    def apply(fields: Option[JField]*)(implicit dummy: DummyImplicit, dummy2: DummyImplicit): JObject = {
      val n = JsonNodeFactory.instance.objectNode()
      fields.flatten.foreach(f => n.set(f._1, f._2): ObjectNode)
      n
      //JsonNodeFactory.instance.objectNode().setAll(fields.flatten.map(f => f.name -> f.value).toMap.asJava)
    }
    def unapply(fields: Seq[(String, JValue)]): Option[Seq[(String, JValue)]] = Some(fields)
  }

/*
  val fields = Seq("x" -> 0, "y" -> "hello")
  val fields2: Seq[JField] = fields
  val o1 = JObject(fields)
  val o2 = JObject(Seq("x" -> 0, "y" -> "hello"))
  val o3 = JObject("x" -> 0)
  val o4 = JObject("x" -> 0, Some("y" -> "123"), None)
  val o5 = JObject(JField("x", 0), "x2" -> 2, Some("y" -> "123"), None, Some(JField("y2", "12345")))
 */

/*
  val optional: Option[Boolean] = Some(true)
  val o: JObject = JObject(
    "f1" -> "f1",
    "f2" -> 0,
    "arr" -> Seq(1, 2, 3, 4, Seq(1, 2, 3), JObject(
      "inner", 0.6
    )),
    optional.map("opt" -> _),
    None
  )

  val f2: BigInt = o \ "f2" match {
    case JInt(i) => i.toInt
  }
  val flattenedArray: Iterable[Double] = (o \ "arr" match {
    case JArray(arr) => arr.collect {
      case x if (x \ "inner" match {
        case JDouble(_) => true;
        case _ => false
      }) => Seq((x \ "inner").doubleValue())
      case JInt(i) => Seq(i.doubleValue())
      case JArray(innerArr) => innerArr.map(_.doubleValue())
    }
  }).flatten
 */

  /*
  val optional: Option[Boolean] = Some(true)
  val n: JObject = JsonNodeFactory.instance.objectNode()
  Seq(
    Some(JField("f1", JsonNodeFactory.instance.textNode("f1"))),
    Some(JField("f2", JInt(0))),
    Some(JField("arr", JArray(Seq(JInt(1), JInt(2), JInt(3), JInt(4), JArray(Seq(JInt(1), JInt(2), JInt(3))),
      {
        val n = JsonNodeFactory.instance.objectNode()
        n.set("inner", JsonNodeFactory.instance.numberNode(0.6).asInstanceOf[JDouble])
      }
    )))),
    optional.map(opt => JField("opt", JsonNodeFactory.instance.booleanNode(opt))),
    None
  ).flatten.foreach(f => n.set(f._1, f._2))
  n
   */

  /*
  JsonNodeFactory.instance.objectNode().setAll(Seq(
    JField("f1", JString("f1")),
    JField("f2", JInt(0)),
    JField("f2", JInt(0)),
  ).map(f => f.name -> f.value).toMap.asJava)
   */




  object JNull {
    def apply(): JNull = {
      JsonNodeFactory.instance.nullNode()
    }
  }
  object JString {
    def apply(s: String): JString = {
      JsonNodeFactory.instance.textNode(s)
    }
    def unapply(s: JString): Option[String] = Some(s.textValue())
  }
  object JInt {
    def apply(i: Int): JInt = {
      JsonNodeFactory.instance.numberNode(BigInt(i).bigInteger).asInstanceOf[BigIntegerNode]
    }
    def apply(i: Short): JInt = {
      JsonNodeFactory.instance.numberNode(BigInt(i).bigInteger).asInstanceOf[BigIntegerNode]
    }
    def apply(i: Long): JInt = {
      JsonNodeFactory.instance.numberNode(BigInt(i).bigInteger).asInstanceOf[BigIntegerNode]
    }
    def apply(i: BigInt): JInt = {
      JsonNodeFactory.instance.numberNode(i.bigInteger).asInstanceOf[BigIntegerNode]
    }
    def unapply(v: NumericNode): Option[BigInt] = v match {
      case _: IntNode|_: ShortNode|_: LongNode|_: DecimalNode|_: BigIntegerNode => Some(v.bigIntegerValue())
      case _ => None
    }
  }
  object JDouble {
    def apply(i: Float): JDouble = {
      JsonNodeFactory.instance.numberNode(i.toDouble).asInstanceOf[JDouble]
    }
    def apply(i: Double): JDouble = {
      JsonNodeFactory.instance.numberNode(i).asInstanceOf[JDouble]
    }
    def apply(i: BigDecimal): JDouble = {
      JsonNodeFactory.instance.numberNode(i.toDouble).asInstanceOf[JDouble]
    }
    def unapply(v: NumericNode): Option[Double] = v match {
      case _: DoubleNode|_: FloatNode => Some(v.doubleValue())
      case _ => None
    }
  }
  object JBool {
    def apply(b: Boolean): JBool = {
      JsonNodeFactory.instance.booleanNode(b)
    }
    def unapply(jb: JBool): Option[Boolean] = Some(jb.booleanValue())
  }
  object JArray {
    def apply(els: Seq[JValue]): JArray = {
      val arrayNode = JsonNodeFactory.instance.arrayNode(els.size)
      els.foreach(arrayNode.add)
      arrayNode
    }
    def unapply(arr: JArray): Option[Iterable[JValue]] = Some(arr.elements().asScala.toIterable)
  }

  object Jckson {
    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)

    val yamlMapper = new ObjectMapper(new YAMLFactory())

    def mapToJson(value: Any): JsonNode = {
      mapper.valueToTree(value)
    }
    def serialize(value: Any): String = {
      import java.io.StringWriter
      val writer = new StringWriter()
      mapper.writeValue(writer, value)
      writer.toString
    }


    def clearMap(_map: java.util.Map[String, Object]): java.util.HashMap[String, Object] = {
      val map = new java.util.HashMap[String, Object]()
      val it = _map.entrySet().iterator()
      while (it.hasNext) {
        val e = it.next()

        map.put(e.getKey, clearEl(e.getValue))
      }
      map
    }
    private def clearEl(el: Object): Object = el match {
      case i: java.lang.Integer => new java.lang.Long(i.toLong)
      case l: java.math.BigInteger => new java.lang.Long(l.longValue())
      case s: java.lang.Short => new java.lang.Long(s.toLong)
      case f: java.lang.Float => new java.lang.Double(f.toDouble)
      case i: BigInt => new java.lang.Long(i.toLong)
      case map: java.util.Map[String, Object] => clearMap(map)
      case map: Map[String, Object] => clearMap(map.asJava)
      case l: java.util.List[_] =>
        val _l = new util.LinkedList[Object]()
        val it = l.iterator()
        while (it.hasNext) {
          val el = it.next().asInstanceOf[Object]
          _l.add(clearEl(el))
        }
        _l
      case l: List[Object] => l.map(clearEl).asJava
      case x => x
    }

    def clearFlatMap(_map: java.util.Map[String, Object],
                     parentKeyMap: Option[(String, java.util.HashMap[String, Object])] = None,
                     arrayKeys: Seq[String] = Nil, filter: Seq[String] = Nil
                    ): java.util.HashMap[String, Object] = {
      val map = parentKeyMap.map(_._2).getOrElse(new java.util.HashMap[String, Object]())
      val it = _map.entrySet().iterator()
      while (it.hasNext) {
        val e = it.next()

        if (!filter.contains(e.getKey)) {
          val key = parentKeyMap.map(k => "%s/%s".format(k._1, e.getKey)).getOrElse(e.getKey)
          clearFlatEl(key, map, arrayKeys, e.getValue)
        }
        //map.put(e.getKey, clearFlatEl(e.getValue))
      }
      map
    }
    private def clearFlatEl(key: String, parentMap: java.util.HashMap[String, Object],
                    arrayKeys: Seq[String],
                    el: Object, filter: Seq[String] = Nil): Unit = el match {
      case i: java.lang.Integer => parentMap.put(key, new java.lang.Long(i.toLong))
      case l: java.math.BigInteger => parentMap.put(key, new java.lang.Long(l.longValue()))
      case s: java.lang.Short => parentMap.put(key, new java.lang.Long(s.toLong))
      case f: java.lang.Float => parentMap.put(key, new java.lang.Double(f.toDouble))
      case i: BigInt => parentMap.put(key, new java.lang.Long(i.toLong))
      case map: java.util.Map[String, Object] => clearFlatMap(map, Some(key -> parentMap), arrayKeys, filter)
      case map: Map[String, Object] => clearFlatMap(map.asJava, Some(key -> parentMap), arrayKeys, filter)
      case l: java.util.List[_] =>
        //val _l = new util.LinkedList[Object]()
        val it = l.iterator()
        var i = 0L
        while (it.hasNext) {
          val el = it.next().asInstanceOf[Object]
          val (arrayKey, filter) = if (arrayKeys.nonEmpty)
            getFlatArrayKey(el, arrayKeys, i)
          else (i.toString, Nil)

          Logger.debug("filter: " + filter)

          clearFlatEl("%s/%s".format(key, arrayKey), parentMap, arrayKeys, el, filter)
          i += 1
        }
      case l: List[Object] =>
        var i = 0L
        l.foreach(el => {
          val (arrayKey, filter) = if (arrayKeys.nonEmpty)
            getFlatArrayKey(el, arrayKeys, i)
          else (i.toString, Nil)

          Logger.debug("filter: " + filter)

          clearFlatEl("%s/%s".format(key, arrayKey), parentMap, arrayKeys, el, filter)
          i += 1
        })
      case x => parentMap.put(key, x)
    }
    private def getFlatArrayKey(el: Object, arrayKeys: Seq[String], i: Long): (String, Seq[String]) = el match {
      case map: java.util.Map[String, Object] =>
        val keys = arrayKeys.flatMap(key => {
          val v = map.get(key)
          if (v != null) v match {
            case _: java.util.Map[String, Object] | _: Map[String, Object] | _: java.util.List[_] | _: List[Object] => None
            case x => Some(key -> "%s=%s".format(key, x.toString))
          } else None
        })
        if (keys.isEmpty) i.toString -> Nil
        else keys.map(_._2).mkString("_") -> keys.map(_._1)
      case map: Map[String, Object] =>
        val keys = arrayKeys.flatMap(key => {
          map.get(key).flatMap(_ match {
            case _: java.util.Map[String, Object] | _: Map[String, Object] | _: java.util.List[_] | _: List[Object] => None
            case x => Some(key -> "%s=%s".format(key, x.toString))
          })
        })
        if (keys.isEmpty) i.toString -> Nil
        else keys.map(_._2).mkString("_") -> keys.map(_._1)
      case _ => i.toString -> Nil
    }

    //ArrayList<LinkedHashMap<?, ?>> companymap = mapper.readValue(jsonCompany, ArrayList.class);
    def parse(value: String): JValue = mapper.readTree(value)
    def parse(value: InputStream): JValue = mapper.readTree(value)
    def parse(value: File): JValue = mapper.readTree(value)

    def parseYaml(value: String): JValue = yamlMapper.readTree(value)
    def parseYaml(value: InputStream): JValue = yamlMapper.readTree(value)
    def parseYaml(value: File): JValue = yamlMapper.readTree(value)

    def deserialize[T: Manifest](value: String): T = {
      val jt = mapper.constructType(typeFromManifest(manifest[T]))
      mapper.readValue(value, jt)
    }

    def deserialize[T: Manifest](value: InputStream) : T ={
      val jt = mapper.constructType(typeFromManifest(manifest[T]))
      mapper.readValue(value, jt)
    }

    def deserialize[T: Manifest](value: File) : T ={
      val jt = mapper.constructType(typeFromManifest(manifest[T]))
      mapper.readValue(value, jt)
    }

    /*private [this] def typeReference[T: Manifest] = new TypeReference[T] {
      override def getType = typeFromManifest(manifest[T])
    }*/

    private [this] def typeFromManifest(m: Manifest[_]): Type = {
      if (m.typeArguments.isEmpty) { m.runtimeClass }
      else new ParameterizedType {
        def getRawType: Class[_] = m.runtimeClass
        def getActualTypeArguments: Array[Type] = m.typeArguments.map(typeFromManifest).toArray
        def getOwnerType: Null = null
      }
    }
  }
  object JSONParser {
    def parse(value: String): Box[JValue] = Helpers.tryo{Jckson.parse(value)}
    def parse(value: InputStream): Box[JValue] = Helpers.tryo{Jckson.parse(value)}
    def parse(value: File): Box[JValue] = Helpers.tryo{Jckson.parse(value)}
  }
}
