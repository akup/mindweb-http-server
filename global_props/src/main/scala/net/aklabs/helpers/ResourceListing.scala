package net.aklabs.helpers

object ResourceListing {
/*
  import java.io.IOException
  import java.net.{URISyntaxException, URLDecoder}
  import java.util.jar.JarFile

  @throws[URISyntaxException]
  @throws[IOException]
  def getResourceListing(clazz: Class[_], path: String): Array[String] = {
    var dirURL = clazz.getClassLoader.getResource(path)
    if (dirURL != null && dirURL.getProtocol == "file") /* A file path: easy enough */
      return new Array(dirURL.toURI)
    if (dirURL == null) {
      /*
              * In case of a jar file, we can't actually find a directory.
              * Have to assume the same jar as clazz.
              */
      val me = clazz.getName.replace(".", "/") + ".class"
      dirURL = clazz.getClassLoader.getResource(me)
    }
    if (dirURL.getProtocol == "jar") {
      /* A JAR path */
      val jarPath = dirURL.getPath.substring(5, dirURL.getPath.indexOf("!")) //strip out only the JAR file
      val jar = new JarFile(URLDecoder.decode(jarPath, "UTF-8"))
      val entries = jar.entries //gives ALL entries in jar
      val result = new Nothing //avoid duplicates in case it is a subdirectory
      while ( {
        entries.hasMoreElements
      }) {
        val name = entries.nextElement.getName
        if (name.startsWith(path)) { //filter according to the path
          var entry = name.substring(path.length)
          val checkSubdir = entry.indexOf("/")
          if (checkSubdir >= 0) { // if it is a subdirectory, we just return the directory name
            entry = entry.substring(0, checkSubdir)
          }
          result.add(entry)
        }
      }
      return result.toArray(new Array[String](result.size))
    }
    throw new UnsupportedOperationException("Cannot list files for URL " + dirURL)
  }
 */
}
