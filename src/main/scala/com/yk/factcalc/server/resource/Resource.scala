package com.yk.factcalc.server.resource

import com.yk.factcalc.logger.Logger
import com.yk.factcalc.server.Callback
import com.yk.factcalc.server.request.{Get, Post, HttpMethod}
import com.yk.factcalc.server.response.HttpResponse

import scala.concurrent.{ExecutionContext, Future}

/**
 * This represents a server resource which is used to register request-handle-response
 * paths.
 * @param ec Implicit execution context for Futures
 * @param logger Implicit logger
 */
class Resource(implicit val ec: ExecutionContext, implicit val logger: Logger) {
	// default 404 handler
	val defaultNoRoute = { request =>
		Future.successful(response.status(404).plain("Not Found"))
	}: Callback

	// default server error handler
	val defaultError = { request =>
		request.error match {
			case _ => Future.successful(response.status(500).plain("Internal Server Error"))
		}
	}: Callback

	private[server] var routes = Seq[(HttpMethod, PathRegex, Callback)]()
	private[server] var noRoute: Option[Callback] = Some(defaultNoRoute)
	private[server] var error: Option[Callback] = Some(defaultError)

	/**
	 * Register HTTP GET request.
	 * @param path GET URI path
	 * @param callback A callback to execute on request
	 * @return No useful value
	 */
	def get(path: String)(callback: Callback) { addTrack(Get, path)(callback) }

	/**
	 * Register HTTP POST request.
	 * @param path POST URI path
	 * @param callback A callback to execute on request
	 * @return No useful value
	 */
	def post(path: String)(callback: Callback) { addTrack(Post, path)(callback) }

	/**
	 * Install 404 handler.
	 * @param callback A callback to execute when route is not found
	 */
	def noRoute(callback: Callback) {
		noRoute = Option(callback)
	}

	/**
	 * Install server error handler.
	 * @param callback A callback to execute on server errors
	 */
	def error(callback: Callback) {
		error = Option(callback)
	}

	/**
	 * Add specific route.
	 * @param method HTTP request method
	 * @param path URI path
	 * @param callback A callback to execute
	 * @return No interesting value
	 */
	private def addTrack(method: HttpMethod, path: String)(callback: Callback): Unit = {
		val regex = PathRegexer(path)
		routes = routes ++ Seq((method, regex, callback))
	}

	/**
	 * Produces a new instance of HTTP response.
	 * @return An instance of HTTP response
	 */
	def response = new HttpResponse
}