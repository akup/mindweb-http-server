package net.aklabs.helpers

import java.io.{File, InputStream}
import java.lang.reflect.{ParameterizedType, Type}
import java.util

import collection.JavaConverters._
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.{IntNode, _}
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.google.common.collect.{MapDifference, Maps}
import net.aklabs.helpers.JsonHelpers.JInt
import org.pmw.tinylog.Logger

import scala.language.implicitConversions

class RichJNode(val jv: JsonNode) {
  def \(name: String): JsonNode = {
    if (jv == null) null
    else jv.get(name) match {
      case i: IntNode => JInt(i.intValue())
      case i: LongNode => JInt(i.longValue())
      case i: ShortNode => JInt(i.shortValue())
      case i: DecimalNode => JInt(i.bigIntegerValue())
      case x => x
    }
  }
  def toOpt: Option[JsonNode] = jv match {
    case _: MissingNode => None
    case _: NullNode => None
    case json     => Some(json)
  }
  def children: Seq[JsonHelpers.JField] = jv match {
    case obj: ObjectNode => obj.fields().asScala.toSeq.map(e => e.getKey -> e.getValue)
    case _ => Nil
  }
  def fields: Seq[JsonHelpers.JField] = children
  def +(jf: JsonHelpers.JField) : JsonNode = jv match {
    case obj: ObjectNode => JsonHelpers.JObject(
      obj.fields().asScala.toSeq.map(e => e.getKey -> e.getValue)
    ).set(jf.name, jf.value)
    case _ => jv
  }
  def +=(jf: JsonHelpers.JField) : JsonNode = jv match {
    case obj: ObjectNode => obj.set(jf.name, jf.value)
    case _ => jv
  }

  def diff(obj: JsonHelpers.JObject): MapDifference[String, AnyRef] = jv match {
    case thisObj: ObjectNode =>
      val leftMap = JsonHelpers.Jckson.clearMap(
        JsonHelpers.Jckson.mapper.convertValue(thisObj, classOf[java.util.HashMap[String, Object]])
      )
      val rightMap = JsonHelpers.Jckson.clearMap(
        JsonHelpers.Jckson.mapper.convertValue(obj, classOf[java.util.HashMap[String, Object]])
      )

      Maps.difference(leftMap, rightMap)

    case _ => throw new Exception("Can diff only objects")
  }
  def jsonpDiff(obj: JsonHelpers.JObject,
                arrayKeys: Seq[String] = Nil): MapDifference[String, Object] = jv match {
    case thisObj: ObjectNode =>
      Logger.debug("jsonpDiff")
      val leftMap = JsonHelpers.Jckson.clearFlatMap(
        JsonHelpers.Jckson.mapper.convertValue(thisObj, classOf[java.util.HashMap[String, Object]]),
        arrayKeys = arrayKeys
      )
      val rightMap = JsonHelpers.Jckson.clearFlatMap(
        JsonHelpers.Jckson.mapper.convertValue(obj, classOf[java.util.HashMap[String, Object]]),
        arrayKeys = arrayKeys
      )

      Logger.debug("jsonpDiff")
      Logger.debug("left:")
      Logger.debug(leftMap)
      Logger.debug("right:")
      Logger.debug(rightMap)

      Maps.difference(leftMap, rightMap)
    case _ => throw new Exception("Can diff only objects")
  }
  def jsonpDiffArrays(obj: JsonHelpers.JObject) =
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

  implicit def field2tuple(field: JField): (String, JValue) = field.name -> field.value
  implicit def tuple2field(fv: (String, JValue)): JField = JField(fv._1, fv._2)
  implicit def fieldseq2tupleseq(fields: Seq[JField]): Seq[(String, JValue)] = fields.map(field2tuple)
  implicit def tupleseq2fieldseq(fv: Seq[(String, JValue)]): Seq[JField] = fv.map(tuple2field)
  implicit def node2rich(jv: JValue): RichJNode = new RichJNode(jv)
  implicit def rich2node(rich: RichJNode): JValue = rich.jv
  implicit def array2rich(array: JArray): RichJArray = new RichJArray(array)
  implicit def rich2array(rich: RichJArray): JArray = rich.array
  implicit def fieldseq2object(fields: Seq[JField]): JObject = JObject(fields)

  def any2JValue(v: Any, error_v: String = "v"): JValue = v match {
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

  case class JField(name: String, value: JValue)

  object JObject {
    def apply(field: (String, JValue)): JObject = apply(Seq(field))
    def apply(fields: Seq[(String, JValue)]): JObject = {
      JsonNodeFactory.instance.objectNode().setAll(fields.toMap.asJava)
    }
    def apply(fields: Map[String, JValue]): JObject = {
      JsonNodeFactory.instance.objectNode().setAll(fields.asJava)
    }
    def unapply(fields: Seq[(String, JValue)]): Option[Seq[(String, JValue)]] = Some(fields)
  }
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
      case l: List[Object] => l.map(clearEl(_)).asJava
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
