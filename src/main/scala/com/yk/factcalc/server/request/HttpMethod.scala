package com.yk.factcalc.server.request

import com.yk.factcalc.server.request.HTTPMessageIOUtils.StartLine

sealed abstract class HttpMethod(val name: String)
case object Get extends HttpMethod("GET")
case object Post extends HttpMethod("POST")
case object Put extends HttpMethod("PUT")
case object Delete extends HttpMethod("DELETE")

object HttpMethod {
	def apply(name: String) = {
		name match {
			case "GET"    => Get
			case "POST"   => Post
			case "PUT"    => Put
			case "DELETE" => Delete
			case _        => new HttpMethod(">:->") {}
		}
	}

	def unapply(startLine: StartLine): Option[HttpMethod] = {
		Some(apply(startLine.method))
	}
}