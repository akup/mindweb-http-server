package net.aklabs.modest

import java.io.File
import java.util.concurrent.Executors

import org.junit.Assert._
import org.junit._
import org.junit.runners.MethodSorters
import org.pmw.tinylog.{Configurator, Level, Logger}

/**
  * Created by Ivan on 09.03.2017.
  */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class TestModest {
  @Before def init(): Unit = {
    Configurator.currentConfig().level(Level.TRACE).activate()
    System.setProperty("curl.verbose", "true")
  }
  //@After def after() = cg.finalize()

  private def getTokenFromString(baseStr: String, token: String): (Int, String) =  {
    val tokenInd = baseStr.indexOf(token)
    if (tokenInd < 0) {
      val statusStr = baseStr.substring(baseStr.indexOf("status")+8);
      statusStr.substring(0, 1).toInt -> ""
    } else {
      val tokenStr = baseStr.substring(tokenInd+token.length()+3)

      0 -> tokenStr.substring(0, tokenStr.indexOf('"'))
    }
  }

  def time[A](name:String, f: => A): A = {
    val t0 = System.currentTimeMillis()
    Logger.info("start "+name)
    val r = f
    Logger.info(name+" time:{} ms "+ (System.currentTimeMillis() - t0))
    r
  }

    //@Test
  /*
  def testDelay() = {
    println("=========START testDelay=============")
    //для теста нужно выставить таймауты до 10 секунд в CurlGlue
    //val cg = Curl.nextConnection
    assertEquals(0, Curl.setopt(CURL.OPT_URL, "https://httpbin.org/delay/10"))
    assertEquals(0, Curl.performBlocking(cg))
    Curl.returnConnection(cg)

    println("===============END==========")
  }

//  @Test
  def test1Get() = {
    println("=========START test1Get=============")
    val cg = Curl.nextConnection
    assertEquals(0, Curl.setopt(cg, CURL.OPT_URL, "https://httpbin.org/get"))
    val t0 = System.currentTimeMillis
    assertEquals(0, Curl.performBlocking(cg))
//    println(cg.getData)

    val t1 = System.currentTimeMillis
    assertEquals(0,Curl.setopt(cg, CURL.OPT_URL, "https://httpbin.org/get?aaa=123"))
    assertEquals(0,Curl.performBlocking(cg))
    println(cg.getContentType)
    println(new String(cg.getContent))
    val t2 = System.currentTimeMillis
    println(" first get: %d ms;  second get %d ms".format(t1 - t0, t2 - t1))
    println("===============END==========")

    println("=========START Post=============")
    val m = new java.util.HashMap[String, String]()
    m.put("Content-Type", "text/plain;charset=UTF-8")
    cg.setHeaders(m)
    val text = "some test text"
    assertEquals(0, Curl.setopt(cg, CURL.OPT_POSTFIELDS, text))
    assertEquals(0, Curl.setopt(cg, CURL.OPT_URL, "https://httpbin.org/post"))
    assertEquals(0, Curl.performBlocking(cg))
    assertEquals(200, cg.getResponseCode)
    println(cg.getContentType)
    println(new String(cg.getContent))

    assertEquals(0,Curl.setopt(cg, CURL.OPT_HTTPGET, 1))
    assertEquals(0,Curl.setopt(cg, CURL.OPT_URL, "https://httpbin.org/get"))
    assertEquals(0, Curl.performBlocking(cg))
    println("===============END==========")

  }
*/

  @Test
  def testModestScala(): Unit = {
    println("=========START testModestScala=============")
    val t0 = System.currentTimeMillis()
    val modest = new ModestAPI()
    val tree = modest.parse("<div><p id=p1>Hello</p><p id=p2><p id=p3><a>link</a><p id=p4><p id=p5><p id=p6></div>")
    val nodes = modest.selectNodes(tree, "div > :nth-child(2n+1):not(:has(a))")

    Logger.debug("Got nodes for selector '%s': %d".format(
      "div > :nth-child(2n+1):not(:has(a))", nodes.length))
    nodes.reverse.foreach(node => {
      Logger.debug("Tag %s".format(modest.jni_node_name(node)))
      val attributes = modest.jni_attributes(node);

      Logger.debug("attributes: %d".format(attributes.length))
      attributes.foreach(attribute =>
        Logger.debug(attribute.toString())
      )

      Logger.debug("Attribute by name")
      Logger.debug(modest.jni_attribute(node, "id").toString())

      Logger.debug("Prepending child html")
      modest.addRowHtml(tree, node, "{{>sitemap/login.mustache}}")

      Logger.debug("Node with prepended html")
      Logger.debug(modest.jni_print_node_tree(node))

      //Logger.debug(jni_print_node(node));
    })

    Logger.debug("Tree with prepended html")
    Logger.debug(modest.jni_print_row_with_insertions(tree))
    //Logger.debug(modest.jni_print_tree(tree))

    modest.cleanup_tree(tree)
    val t1 = System.currentTimeMillis()

    println("time 1: %d ms".format(t1-t0))

    println("===============END==========")
  }

  @Test
  def testModestFileScala(): Unit = {
    println("=========START testModestScala file parse=============")
    val t0 = System.currentTimeMillis()
    val modest = new ModestAPI()
    val tree1 = modest.parseFile("./test/loginroot.mustache")
    val tree2 = modest.parseFile("./test/login.mustache")

    Logger.debug(modest.jni_print_tree(tree1))
    Logger.debug(modest.jni_print_tree(tree2))

    modest.cleanup_tree(tree1)
    modest.cleanup_tree(tree2)

    val t1 = System.currentTimeMillis()

    println("time 1: %d ms".format(t1-t0))

    println("===============END==========")
  }

//  @Test
  /*
  def test5CurlScala(): Unit = {
    println("=========START test5CurlScala=============")
    println("secureLogin")
    var res = Curl.execute("http://stage.nexpo.me/auth/login", postForm = Map("email"->"pdpopd@gmail.com","pass"->""))
    val token = getTokenFromString(new String(res.content), "temptoken")
    val param = Map("ak-fivesec-token" -> token._2, "ak-fivesec-token-email" -> "pdpopd@gmail.com")
    res = Curl.execute("http://stage.nexpo.me", getParams = param)
    println("===============END==========")
  }

//  @Test
  def test6WebDav():Unit = {
    println("=========START test6WebDav=============")
    val url = "http://admin:archivaPSW99@repo.nexpo.me/repository/registration/"
    val h = Map("DEPTH" -> "1")

    var res = Curl.execute("http://admin:archivaPSW99@repo.nexpo.me/repository/registration/", headers = h, customRequest = Some("PROPFIND"))
    println(res)
    println(res.asString)

    res = Curl.execute("http://admin:archivaPSW99@repo.nexpo.me/repository/registration/0.0.1-TEST/arm.tar.gz", inFile = true)
    println(res)
    val f = new File(res.fileName.get)
    println("f.exists "+f.exists())
    println("f.length "+f.length()/1024.0/1024.0+" mb")
    f.delete()
    println("===============END==========")
  }

//    @Test
  /*
  def test7():Unit = {
    println("=========START test7=============")
    val url2 = "http://admin:archivaPSW99@repo.nexpo.me/repository/registration/0.0.1-TEST/arm.tar.gz"

    val cgs = Seq(new CurlGlue, new CurlGlue, new CurlGlue)
    cgs.foreach(_.init())

    val v = Executors.newFixedThreadPool(8)

    def load(i:Int) = {
      new Runnable {
        override def run(): Unit = {
          val res = Curl.execute(cgs(i), url2 , inFile = true)
          val f = new File(res.fileName.get)
          println("f.exists "+f.exists())
          println("f.length "+f.length()/1024.0/1024.0+" mb")
          f.delete()
        }
      }
    }

    v.execute(load(1))
    v.execute(load(1))
    v.execute(load(2))

    println("===============END==========")
  }
   */
*/
}