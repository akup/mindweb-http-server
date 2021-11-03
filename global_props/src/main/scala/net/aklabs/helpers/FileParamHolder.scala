package net.aklabs.helpers

sealed trait ParamHolder extends Serializable{
  def name: String
}

final case class NormalParamHolder(name: String, value: String) extends ParamHolder

abstract class NonBlockingData {
  def release(): Unit
  def readAvailable(doneFunc: () => Unit,
                     breakFunc: () => Unit,
                     readFunc: (Array[Byte], Boolean) => Unit,
                     waitFunc: () => Unit): Unit
}
/**
 * A FileParamHolder contains a file uploaded via a multipart
 * form.
 *
 * @param name The name of the form field for this file
 * @param mimeType the mime type, as specified in the Content-Type field
 * @param fileName The local filename on the client
 */
abstract class FileParamHolder(val name: String, val mimeType: String,
                               val fileName: String) extends ParamHolder with Serializable
{
  var complete: Boolean = true
  /**
   * Returns an input stream that can be used to read the
   * contents of the uploaded file.
   */
  def data: NonBlockingData

  /**
   * Returns the length of the uploaded file.
   */
  def length : Long
}
