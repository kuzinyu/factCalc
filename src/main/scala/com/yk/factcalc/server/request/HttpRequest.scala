package com.yk.factcalc.server.request

import com.yk.factcalc._

import java.net.{InetSocketAddress, Socket}

trait HttpRequest {
	val method: HttpMethod
	val uri: String
	val version: String
	val header: Map[String, String]
	val params: Map[String, String]
	val payload: Array[Byte]

	val socketAddress: InetSocketAddress
	val address = socketAddress.getAddress
	val host = address.getHostAddress
	val port = socketAddress.getPort

	val uriPath = {
		uri.indexOf('?') match {
			case -1 => uri
			case n  => uri.substring(0, n)
		}
	}

	var error: Option[Throwable] = None

	def getHeaderParam(name: String) = header.get(name).orNull
	def getHeaderParam(name: String, fallback: String) = header.getOrElse(name, fallback)

	def getIntParam(name: String, fallback: Int = 0) =
		params.get(name)
					.map(str2Int(_))
					.getOrElse(fallback)


	override def toString = s"(${method.name} $uri $version) from $socketAddress"
}

object HttpRequest {

	/**
	 * Given a socket produce a request object.
	 * @param socket A socket to read request data from
	 * @return A request object from the given socket
	 */
	def apply(socket: Socket): HttpRequest = {
		require(socket != null, "socket is null!")

		HTTPMessageIOUtils.parseRequest(socket)
	}

	/**
	 * Produces a request object from the arguments given.
	 * @param _method HTTP method
	 * @param _uri Request URI
	 * @param _version HTTP version
	 * @param _headerParams Request header parameters
	 * @param _requestParams Request parameters
	 * @param _socketAddress An instance of the InetSocketAddress class
	 * @param _payload A binary payload of the request
	 * @return
	 */
	def apply(_method: HttpMethod, _uri: String, _version: String,
	          _headerParams: Map[String, String], _requestParams: Map[String, String],
	          _socketAddress: InetSocketAddress, _payload: Array[Byte]) = {
		new {
			val method = _method
			val uri = _uri
			val version = _version
			val header = _headerParams
			val params = _requestParams
			val socketAddress = _socketAddress
			val payload = _payload
		} with HttpRequest
	}

	def apply(): HttpRequest = HttpRequest(HttpMethod(""), "", "", Map.empty, Map.empty, new InetSocketAddress(0), Array.empty)
}