package net.aklabs.http

import net.aklabs.helpers.{FileParamHolder, NonBlockingData}

class StreamFileParamHolder(override val name: String, override val mimeType: String,
                            override val fileName: String,
                            dataIn: NonBlockingData,
                            size: Long) extends FileParamHolder(name, mimeType, fileName) {
  {
    complete = false
  }
  /**
   * Returns an input stream that can be used to read the
   * contents of the uploaded file.
   */
  override def data: NonBlockingData = dataIn

  /**
   * Returns the length of the uploaded file.
   */
  override def length: Long = size
}


/*
/**
 * This FileParamHolder stores the uploaded file directly into memory.
 *
 * @param name The name of the form field for this file
 * @param mimeType the mime type, as specified in the Content-Type field
 * @param fileName The local filename on the client
 * @param file The contents of the uploaded file in a byte array
 */
class InMemFileParamHolder(override val name: String, override val mimeType: String,
                           override val fileName: String, val file: Array[Byte]) extends
FileParamHolder(name, mimeType, fileName)
{
  /**
   * Returns an input stream that can be used to read the
   * contents of the uploaded file.
   */
  def fileStream: InputStream = new ByteArrayInputStream(file)

  /**
   * Returns the length of the uploaded file.
   */
  def length : Long = if (file == null) 0 else file.length
}


/**
 * This FileParamHolder stores the uploaded file in a
 * temporary file on disk.
 * 
 * @param name The name of the form field for this file
 * @param mimeType the mime type, as specified in the Content-Type field
 * @param fileName The local filename on the client
 * @param localFile The local copy of the uploaded file
 */
class OnDiskFileParamHolder(override val name: String, override val mimeType: String,
                            override val fileName: String, val localFile: File) extends
FileParamHolder(name, mimeType, fileName)
{
  /**
   * Returns an input stream that can be used to read the
   * contents of the uploaded file.
   */
  def fileStream: InputStream = new FileInputStream(localFile)

  /**
   * Returns the contents of the uploaded file as a Byte array.
   */
  def file: Array[Byte] = IOUtils.toByteArray(fileStream)

  /**
   * Returns the length of the uploaded file.
   */
  def length : Long = if (localFile == null) 0 else localFile.length

  protected override def finalize() {
    tryo(localFile.delete)
  }
}

object OnDiskFileParamHolder {
  def apply(n: String, mt: String, fn: String, inputStream: InputStream): OnDiskFileParamHolder =
  {
    val file: File = File.createTempFile("lift_mime", "upload")
    val fos = new FileOutputStream(file)
    val ba = new Array[Byte](8192)
    @scala.annotation.tailrec
    def doUpload() {
      inputStream.read(ba) match {
        case x if x < 0 =>
        case 0 => doUpload()
        case x => fos.write(ba, 0, x); doUpload()
      }

    }

    doUpload()
    inputStream.close()
    fos.close()
    new OnDiskFileParamHolder(n, mt, fn, file)
  }
}

object FileParamHolderBuilder {
  def apply(n: String, mt: String, fn: String, file: Array[Byte]): FileParamHolder =
  new InMemFileParamHolder(n, mt, fn, file)

  def unapply(in: Any): Option[(String, String, String, Array[Byte])] = in match {
    case f: InMemFileParamHolder => Some((f.name, f.mimeType, f.fileName, f.file))
    case _ => None
  }
}
 */