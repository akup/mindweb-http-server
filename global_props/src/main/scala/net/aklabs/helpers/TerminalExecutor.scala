package net.aklabs.helpers

import java.io.{BufferedReader, File, InputStream, InputStreamReader}

import scala.collection.mutable
import scala.util.matching.Regex

object TerminalExecutor {

  final val breakTag = "break"
  final val defaultState = "default"
  //blocks
  //braeks
  def execCommand(command: String, params: Seq[String],
                  parseByTags: Seq[(String, String)] = Nil,
                  fromPath: String = "./"): (Int, Map[String, Seq[String]]) = {
    execCommand(command, params, Map(defaultState ->
      parseByTags.map(tag_expr => (tag_expr._1, tag_expr._2, None))), fromPath)
  }

  private def stateTagsRegex(parseByTagsMap: Map[String, Seq[(String, String, Option[String])]],
                             state: String = defaultState): Seq[(String, Regex, Option[String])] = {
    //sort to make break tag first
    parseByTagsMap(state).sortWith((a, _) => if (a._1 == breakTag) true else false).map(tag_exp =>
      (tag_exp._1, {
        val _temp1 = if (!tag_exp._2.startsWith("^")) "^%s".format(tag_exp._2) else tag_exp._2
        val _temp2 = if (!_temp1.endsWith("$")) "%s$".format(_temp1) else _temp1
        new Regex(_temp2)
      }, tag_exp._3)
    )
  }
  def execCommand(command: String, params: Seq[String],
                  parseByTagsMap: Map[String, Seq[(String, String, Option[String])]],
                  fromPath: String): (Int, Map[String, Seq[String]]) = try {
    val array = command +: params
    //println(array)
    val prc = Runtime.getRuntime.exec(array.toArray, null, new File(fromPath))

    var tagsAndRegex = stateTagsRegex(parseByTagsMap, defaultState)
    var parsedByTags: Map[String, mutable.ListBuffer[String]] = Map.empty

    var breaked = false
    var in: InputStream = null
    var inErr: InputStream = null
    try {
      in = prc.getInputStream
      inErr = prc.getErrorStream
      val inR = new BufferedReader(new InputStreamReader(in))
      val inErrR = new BufferedReader(new InputStreamReader(inErr))

      var _line: String = null
      var _lineErr: String = null

      while(!breaked && {_line = inR.readLine(); _lineErr = inErrR.readLine(); _line != null || _lineErr != null}) {
        val line = if (_line == null) _lineErr else _line
        println("Exec line: " + line)
        tagsAndRegex.find(strRegex => {
          val regex = strRegex._2
          regex.findFirstMatchIn(line).map(matchResult => {
            val tag = strRegex._1
            if (tag == breakTag) {
              prc.destroy()
              breaked = true
            } else {
              val newSeq = if (matchResult.groupCount > 0)
                (1 to matchResult.groupCount).map(matchResult.group)
                else Seq(line)
              parsedByTags.get(strRegex._1) match {
                case Some(seq) => seq ++= newSeq
                case _ => parsedByTags += tag -> mutable.ListBuffer(newSeq: _*)
              }

              strRegex._3.foreach(newKey =>
                tagsAndRegex = stateTagsRegex(parseByTagsMap, newKey)
              )
            }
          }).isDefined
        })
      }
    } finally {
      if (in != null) in.close()
      if (inErr != null) inErr.close()
    }
    (if (breaked) -1 else prc.waitFor(), parsedByTags)
  } catch {
    case e: Throwable => e.printStackTrace(); throw e;
  }
}
