package com.yk.factcalc.server

import java.io.BufferedOutputStream
import java.net.{ServerSocket, Socket}
import java.util.Date

import com.yk.factcalc.logger.Logger
import com.yk.factcalc.server.request.HttpRequest
import com.yk.factcalc.server.resource.Resource
import com.yk.factcalc.server.response.HttpResponse

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.util.{Try, Failure, Success}

/**
 * Represents a server.
 * @param name A name of a server
 * @param port A port to listen incomig connections on
 * @param resource A resource to server requests with
 * @param readTimeoutMs Socket read operations timeout
 * @param ec Implicit execution context for Futures
 * @param logger Implicit logger
 */
class Server(name: String, port: Int, resource: Resource, readTimeoutMs: Int = 10000)
            (implicit val ec: ExecutionContext, implicit val logger: Logger) extends Thread {

	private val notFoundHandler = resource.noRoute.getOrElse(resource.defaultNoRoute)
	private val errorHandler = resource.error.getOrElse(resource.defaultError)

	private val serverSocket = new ServerSocket(port)

	private val thisClass = classOf[Server].getSimpleName
	private val serverSays = s"$thisClass - $name"

	/**
	 * Services client connections, a read-handle-respond cycle.
	 */
	@tailrec
	private def service(): Unit = {
		val clientSocket = serverSocket.accept()
		clientSocket.setSoTimeout(readTimeoutMs)
		logger.info(s"> $serverSays accepted connection from $clientSocket")

		read(clientSocket).flatMap { request =>
			tryHandle(request)
		} flatMap { response =>
			write(clientSocket, response)
		} onComplete {
			complete(clientSocket, _)
		}

		service()
	}

	/**
	 * Reads client socket data and translates it to HTTP request.
	 * @param socket Socket representing a client
	 * @return HTTP request
	 */
	private def read(socket: Socket): Future[HttpRequest] = {
		Future {
			blocking {
				val request = HttpRequest(socket)
				logger.info(s"> $serverSays parsed request as $request")

				request
			}
		}
	}

	/**
	 * Tries to handle HTTP request, does some basic error handling.
	 * @param request A HTTP request to handle
	 * @return
	 */
	private def tryHandle(request: HttpRequest): Future[HttpResponse] = {
		try {
			handle(request).recover {
				case t => Await.result(handleError(request, t), Duration.Inf)
			}
		} catch {
			case e: Exception =>
				handleError(request, e)
		}
	}

	/**
	 * Handles HTTP request calling appropriate callback function.
	 * @param request A HTTP request to handle
	 * @return
	 */
	private def handle(request: HttpRequest): Future[HttpResponse] = {
		// if there's a route registered for the request
		resource.routes.find {
			case (_, regex, _) => regex(request.uriPath).isDefined
		} match {
			case Some((_, _, callback)) => callback(request)
			case None                   => notFoundHandler(request)
		}
	}

	/**
	 * Does some basic error handling.
	 * @param request A failed HTTP request
	 * @param t A cause of a failure
	 * @return HttpResponse which tells something useful to the client
	 */
	private def handleError(request: HttpRequest, t: Throwable) = {
		logger.error(t, s"! $serverSays Internal Server Error")
		request.error = Some(t)
		errorHandler(request)
	}

	/**
	 * Writes HTTP response back to the client.
	 * @param socket Client socket to write response to
	 * @param response HTTP response
	 * @return No useful value
	 */
	private def write(socket: Socket, response: HttpResponse): Future[Unit] = {
		response.server(name)
		response.date(new Date())

		logger.info(s"< $serverSays responding with\n$response")

		Future {
			// convert HttpRequest to a sequence of bytes
			val out = new BufferedOutputStream(socket.getOutputStream)
			val payload = response.build

			logger.info(s"< $serverSays writing ${payload.length} bytes back to $socket")

			blocking {
				// write what's available to the client socket
				out.write(payload)
				out.flush()
			}
		}
	}

	private def complete(socket: Socket, _with: Try[Unit]) = {
		_with match {
			case Success(s) => socket.close()
			case Failure(t) =>
				logger.error(t, s"! $serverSays error while reading $socket")
				socket.close()
		}
	}

	override def run() = {
		logger.info(s"* $serverSays started, listening for incoming connections on port $port *")
		service()
	}
}
