package net.aklabs.regbox

import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import java.util.regex.Pattern

object CsvUtils {
  def processCSV(in: InputStream,
      map_columns: ArrayBuffer[String] => Any,
      on_exception: Throwable => Unit): ListBuffer[Any] = {
    in.reset
    val ir = new InputStreamReader(in, "UTF8");
    val br = new BufferedReader(ir);
    
    var tempLine: String = ""
    var s: String = ""
    
    try {
      var result = ListBuffer[Any]()
      var first_line = true
      while ({ s = br.readLine(); s != null}) {
        val columns = readCsvLine(s, br)
        /*
        var columns = splitFromCSV(s);
        tempLine = s;
        while (columns == None && tempLine != null) {
          tempLine = br.readLine();
          s += tempLine;
          columns = splitFromCSV(s);
        }
        */
        
        if (!first_line) {
          try {
            columns.foreach(m_c => result.append(map_columns(m_c._1)))
            
            //columns
          } catch {
            case e: Throwable =>
          }
        }
        first_line = false
        
      }
      return result
    } catch {
      case e: Throwable => on_exception(e)
    }
    ListBuffer[Any]()
  }
  
  def readCsvLine(firstStr: String, br: BufferedReader, separator:String = ",") = {
    var s = if (firstStr.size > 2 && firstStr.charAt(0).toByte == -1 && firstStr.charAt(1) == '"') {
      firstStr.substring(1)
    } else firstStr
    var columns = splitFromCSV(s, separator);
    var tempLine = s;
    while (columns == None && tempLine != null && br.ready()) {
      tempLine = br.readLine();
      s += tempLine;
      columns = splitFromCSV(s, separator);
      if (columns == None) s += "\n"
    }
    columns.map(_ -> s)
  }
  
  def escapeCSVQuotes(field: String): String = field.replaceAll("\"\"", "\"")

  def splitFromCSV(line: String, separator:String = ","): Option[ArrayBuffer[String]] = {
    if (line == null)
      return None
    var splits = ArrayBuffer[String]()
    var i = 0
    var shift = 0
    var l = line
    while (i < l.length()) {
      var end = -1
      if (l.charAt(i) == '"') {
        var recheck = true
        var search_from = 1
        while (recheck) {
          recheck = false
          end = l.indexOf("\"" + separator, search_from)
          if (end>0) {
            if (countEndQuotes(l, end) % 2 == 0) {
              search_from = end + 1
              end = -1
              recheck = true
            }
          }
        }
          
        shift = 1
      } else {
        end = l.indexOf(separator)
        shift = 0
      }
      
      if (end > -1) {
        splits += escapeQuotes(l.substring(i + shift, end), shift>0)
        l = l.substring(end + 1 + shift)
        i = 0
      } else {
        if (shift != 0) {
          if (countEndQuotes(l, l.length-1) % 2 == 0)
            return None
        }
        splits += escapeQuotes(l.substring(shift, l.length() - shift), shift>0)
        i = l.length()
      }
    }

    return Some(splits)
  }
  
  private def escapeQuotes(str: String, dQuotes: Boolean) = {
    if (dQuotes) str.replaceAll("\"\"", "\"")
    else str
  }
  
  private def countEndQuotes(str: String, startFrom: Int): Int = {
    var quotesCount = 0
    var endInd = startFrom
    
    while (endInd > -1 && str(endInd) == '"') {
      quotesCount += 1
      endInd -= 1
    }
    if (endInd < 0) quotesCount -= 1 
    
    quotesCount
  }

  private val escape_pattern = Pattern.compile("[\";\n]")

  //кажется не используется закомментил 17.11.2016
//  def createLine(vals: List[String]):String = {
//    val escaped = vals.map(v => if (escape_pattern.matcher(v).find()) {
//      "\"" + v.replaceAll("\"","\"\"") + "\""
//    } else v)
//    if (escaped.size > 0) escaped.reduce(_ + ";" + _) else escaped
//  }
}