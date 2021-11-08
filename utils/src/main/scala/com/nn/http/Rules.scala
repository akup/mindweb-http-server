package com.nn.http

import net.aklabs.helpers._

object Rules {
	var throwOnOutOfScopeVarAccess: Boolean = false
	
	var defaultJsScriptLocation = "/js/"
	var defaultJsScripts = Seq(
		"common/jquery-3.5.1.min.js" -> "defer",
		"mindweb/ajax_request.js" -> "defer",
		"mindweb/mw_ajax.js" -> "defer",
		"mindweb/errors.js" -> "async",
		"common/toastr.min.js" -> "defer",
		"common/jquery.scrollTo.js" -> "defer",
		"common/jquery.showmask.js" -> "defer",
		"common/jquery.arcticmodal-0.3.js" -> "defer",
		"common/mw-selectize.js" -> "defer",
		"common/mustache.js" -> "defer",
		"mindweb/templater.js" -> "defer",
		"mindweb/ajax_nav.js" -> "defer")
	var defaultCss = Seq("all" -> "main.css")

	var defaultAngularJsScripts = Seq(
		"runtime.js" -> "defer",
		"polyfills-es5.js" -> "defer",
		"polyfills.js" -> "defer",
		"vendor.js" -> "defer",
		"main.js" -> "defer")
	var defaultAngularJsScriptLocation = "/angjs/"
	
	/**
   * Put a test for being logged in into this function
   */
  @volatile var loggedInTest: Box[() => Boolean] = Empty
}