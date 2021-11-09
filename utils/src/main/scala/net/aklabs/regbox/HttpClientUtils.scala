package net.aklabs.regbox

import java.io._
import java.net.URI

import com.nn.curl.{Curl, CurlCookie, CurlResult}
import net.aklabs.helpers.JsonHelpers.Jckson
import org.apache.commons.io.IOUtils
import org.pmw.tinylog.Logger

object HttpClientUtils {

	//lazy val httpClient = HttpClients.createMinimal

	/*
	val requestConfig = RequestConfig.custom()
			.setConnectionRequestTimeout(10000)
			.setConnectTimeout(10000)
			.setSocketTimeout(30000)
			.build();
			* 
			*/

	def close(){
		Curl.close()
	}

	/**
		* libcurl
		* Запрос по Ssl к серверу за токеном
		*/
	def secureLogin(url: String, params: Map[String, String]):Either[Exception, String] ={
		val v = Curl.execute(url, postForm = params)
		val out = if (v.responseCode == 200){
		  Logger.info("secureLogin", v.asString)
			val token = getTokenFromString(v.asString, "temptoken")
			if (token._1 > 0)
				Left(new Exception("invalid_cred"))
			else
				Right(token._2)
		} else
			Left(new Exception("status code: "+v.responseCode))
		Logger.trace("out={}",out)
		out
	}

	/**
		* libcurl
		* Делаем запрос с токенами к серверу за сессией
		*/
	def logInRequest(url:String, tokens: Option[(String, String)]): Either[Exception,Option[String]] = {
		val param = Map("ak-fivesec-token" -> tokens.get._1, "ak-fivesec-token-email" -> tokens.get._2)
		val resp = Curl.execute(url, getParams = param)
		val out = if (resp.responseCode == 200){
			val jsession = resp.headers.find(_._1.equalsIgnoreCase("set-cookie")).map(h => h._2.substring(h._2.indexOf('=')+1,h._2.indexOf(';')))
			Right(jsession)
		} else
			Left(new Exception("response status "+resp.responseCode))
		Logger.trace("out={}",out)
		out
	}

	/**
		* другой запрос к серверу: сессия уже есть и нас интересует тело
		*/
	def loggedInRequest(url:String, params:Option[Map[String, String]],  jsessionId: Option[String]): Either[java.lang.Exception,
		scala.collection.immutable.Map[java.lang.String,Any]] = {
		//jsessionId.map(jsess => curlGlue.putCookies(getDomainName(url), "JSESSIONID", jsess))
		val resp = Curl.execute(url, getParams = params.getOrElse(Map.empty),
			withCookie = jsessionId.map(jsess =>
				CurlCookie("JSESSIONID", jsess, domain = Some(getDomainName(url)))
			).toSeq)
		Logger.trace("executed")

		val out = if (resp.responseCode == 200) {
		  Logger.trace("executed code: " + resp.responseCode)
			val str:String = resp.asString
			Logger.trace("executed as tring")
			val json = Jckson.deserialize[Map[String, _]](str)
			Logger.trace("deserialisation!!!")
			if (json.contains("errors")){
				Left(new Exception(" "+json.getOrElse("errors", "unknown error")))
			} else
				Right(json)
		} else
			Left(new Exception("response status "+resp.responseCode))
		Logger.trace("out={}",out)
		out
	}

	/**
	* другой запрос к серверу: сессия уже есть и нас интересует тело
	* triesCount - количество попыток запроса
	*/
	def loggedInRequest(url:String, params:Option[Map[String, String]], jsessionId: Option[String], triesCount:Int): Either[java.lang.Exception,
	 scala.collection.immutable.Map[java.lang.String,Any]] = {
		var result: Either[java.lang.Exception, scala.collection.immutable.Map[java.lang.String,Any]] = Left(new java.lang.Exception())
		for(i <- 1 to triesCount; if result.isLeft) {
			// Logger.info("httpClientUtils try # "+i)
			result = loggedInRequest(url, params, jsessionId)
		}
		result
	}

//RegBoxHelpers.getHost() / "boxapi" / "getconf" <<? Map("last_synch" -> last_synch.toString))
//RegBoxHelpers.getHost() / "tracked" / "UniRegUser" <<? Map("from_time" -> last_synch.toString, "json" -> (eid + "_reg_track_request"))
//RegBoxHelpers.getHost() / "boxapi" / "uploadreg" <<? Map("vals" -> serialized_vals))
	/**
		* libcurl
		* запрос с токенами: используется для синхронизации
		*/
	def tokenedRequestStr(url:String, params : Map[String, String], expoUID: Option[String] = None): Either[Throwable, String]={
		Logger.info("tokened request " + BootstrapDI.bootstrapper.map(_.getDeviceId) + " " + BootstrapDI.bootstrapper.map(_.getCurrentExpoUID) + " " +
			BootstrapDI.bootstrapper.map(_.getCurrentExpoToken))

		val either =  (for {
			dev_id <- BootstrapDI.bootstrapper.flatMap(_.getDeviceId)
			eid <- expoUID.orElse(BootstrapDI.bootstrapper.flatMap(_.getCurrentExpoUID))
			token <- BootstrapDI.bootstrapper.flatMap(_.getCurrentExpoToken)
			} yield {
				val form = params + ("dev_id" -> dev_id, "token" -> token, "eid" ->  eid)
				val res = Curl.execute(url, postForm = form)

				if(res.responseCode == 200)
					Right(res.asString)
				else
					Left(new Exception("return status: "+res.responseCode))
			}).getOrElse(Left(new Exception("invalid_cred")))
		Logger.trace("out={}",either)
		either
	}
	
	def tokenedRequestFUpload(url:String, files: Seq[File], params : Map[String, String], expoUID: Option[String] = None): Either[Throwable, String]={
	  val either =  (for {
			dev_id <- BootstrapDI.bootstrapper.flatMap(_.getDeviceId)
			eid <- expoUID.orElse(BootstrapDI.bootstrapper.flatMap(_.getCurrentExpoUID))
			token <- BootstrapDI.bootstrapper.flatMap(_.getCurrentExpoToken)
			} yield {
				val form = params + ("dev_id" -> dev_id, "token" -> token, "eid" ->  eid)
				val res = Curl.execute(url, postFiles = files, postForm = form)

				if(res.responseCode == 200)
					Right(res.asString)
				else
					Left(new Exception("return status: "+res.responseCode))
			}).getOrElse(Left(new Exception("invalid_cred")))
		Logger.trace("out={}",either)
		either
	}

	/**
		* libcurl
		* запрос с токенами: используется для синхронизации
		*/
	def tokenedRequestStrGet(url:String, params : Map[String, String], expoUID: Option[String] = None): Either[Throwable, String]={
		Logger.info("tokened request " + BootstrapDI.bootstrapper.map(_.getDeviceId) + " " + BootstrapDI.bootstrapper.map(_.getCurrentExpoUID) + " " +
			BootstrapDI.bootstrapper.map(_.getCurrentExpoToken))

		val either =  (for {
			dev_id <- BootstrapDI.bootstrapper.flatMap(_.getDeviceId)
			eid <- expoUID.orElse(BootstrapDI.bootstrapper.flatMap(_.getCurrentExpoUID))
			token <- BootstrapDI.bootstrapper.flatMap(_.getCurrentExpoToken)
		} yield {
			val form = params + ("dev_id" -> dev_id, "token" -> token, "eid" ->  eid)
			val res = Curl.execute(url, getParams = form)

			if(res.responseCode == 200)
				Right(res.asString)
			else
				Left(new Exception("return status: "+res.responseCode))
		}).getOrElse(Left(new Exception("invalid_cred")))
		Logger.trace("out={}",either)
		either
	}

	def curlPostStr(url:String, text:String ): CurlResult =  Curl.execute(url, Some(text))

	def tokenedRequestIs(url:String, params : Map[String, String], expoUID: Option[String] = None): Either[Throwable, CurlResult]={
		val either = (
			for {
				dev_id <- BootstrapDI.bootstrapper.flatMap(_.getDeviceId)
				eid <- expoUID.orElse(BootstrapDI.bootstrapper.flatMap(_.getCurrentExpoUID))
				token <- BootstrapDI.bootstrapper.flatMap(_.getCurrentExpoToken)
			} yield {

				val form = params + ("dev_id" -> dev_id, "token" -> token, "eid" -> eid)
				val resp = Curl.execute(url, postForm = form, inFile = true)

				if(resp.responseCode == 200)
					Right(resp)
				else
					Left(new Exception("return status: "+resp.responseCode))
			}).getOrElse(Left(new Exception("invalid_cred")))
      Logger.trace("out={}",either)
		either
	}

	/**
	 * <p>Чтение из потока в строку.
	 *
	 * <p>Внимание: поток после этого не освобождается!
	 *
	 * @param in - inputStream
	 * @return
	 */
	def convertStreamToString(in : InputStream, chs: Option[String] = None):String = {
		IOUtils.toString(in, chs.getOrElse("UTF-8"))
	}

	/**
	 * извлекаем токен из строки
	 *
	 * @param baseStr строка вида [.]"token":"token value"[.]
	 * @param token
	 * @return token value
	 */
	private def getTokenFromString(baseStr: String, token: String): (Int, String) =  {
	  val tokenInd = baseStr.indexOf(token)
	  if (tokenInd < 0) {
	    val statusStr = baseStr.substring(baseStr.indexOf("status")+8)
      statusStr.substring(0, 1).toInt -> ""
	  } else {
	    val tokenStr = baseStr.substring(tokenInd+token.length()+3)

      0 -> tokenStr.substring(0, tokenStr.indexOf('"'))
	  }
	}

/*
	def getContTypeEncoding(entity: HttpEntity) = {
	  val ct = entity.getContentType().getValue()
	  val c_enc = entity.getContentEncoding()
	  if (c_enc == null || c_enc.getValue() == null) {
	    val regex = """([^;]*); *charset *= *(.*)$""".r;
	    ct match {
	      case regex(c_t, ch_s) => c_t -> Some(ch_s)
	      case _ => ct -> None
	    }
	  }
	  else ct -> Some(c_enc.getValue())
	}
	* 
	*/

	def getContTypeEncoding(ct: String) = {
		val regex = """([^;]*); *charset *= *(.*)$""".r;
		ct match {
			case regex(c_t, ch_s) => c_t -> Some(ch_s)
			case _ => ct -> None
		}
	}

	def getDomainName( url:String):String = {
		val domain = new URI(url).getHost
		if (domain.startsWith("www.")) domain.substring(4) else domain
	}

	def fileExistsInWebDav(url: String):Boolean = {
		val res = Curl.execute(url, requestType = Some("PROPFIND"))
		if (res.content.length > 0)
			true
		else
			false
	}

	def loadFile(url:String, file:File):Boolean = {
		Logger.info("Start downloading "+file.getName)
		val res = Curl.execute(url, inFile = true)
		if(res.responseCode == 200) {
			res.fileName.foreach(x => {file.delete(); new File(x).renameTo(file)})
			file.exists()
		} else
			false
	}

}