package net.aklabs.modest

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean

import org.pmw.tinylog.Logger

class ModestTree(val pointer: Long, modest: ModestAPI) {
  private val pointerCleaned: AtomicBoolean = new AtomicBoolean(false)

  def select(selector: String): Array[ModestNode] = {
    modest.selectNodes(pointer, selector).map(new ModestNode(_, pointer, modest))
  }
  override def toString() = {
    modest.jni_print_row_with_insertions(pointer)
  }

  override def finalize(): Unit = {
    if (!pointerCleaned.getAndSet(true))
      modest.cleanup_tree(pointer)
    super.finalize()
  }
}
class ModestNode(val pointer: Long, val parent_tree: Long, modest: ModestAPI) {
  def tagName: String =
    modest.jni_node_name(pointer)
  def attributes: Array[HtmlAttribute] =
    modest.jni_attributes(pointer)
  def prependChildHtml(html: String) = {
    modest.addRowHtml(parent_tree, pointer, html)
  }

  override def toString() = {
    Modest.getModestAPI().jni_print_node_tree(pointer)
  }
}
object Modest {
  private val threadLocalModestApi = new ThreadLocal[ModestAPI]() {
    override protected def initialValue: ModestAPI = {
      new ModestAPI()
    }
  }
  private[modest] def getModestAPI() = threadLocalModestApi.get()

  def parseFile(filePath: String): ModestTree = {
    if (!new File(filePath).exists()) throw new Exception("No file")
    val modest = threadLocalModestApi.get()
    new ModestTree(modest.parseFile(filePath), modest)
  }
}
