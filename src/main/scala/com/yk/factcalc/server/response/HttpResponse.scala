package com.yk.factcalc.server.response

import java.util.Date

import com.yk.factcalc.server.request.HTTPMessageIOUtils.UTF_8
import com.yk.factcalc.server.request.HTTPMessageIOUtils.US_ASCII
import com.yk.factcalc._

import scala.collection.mutable.ArrayBuffer

/**
 * Represents HTTP response.
 */
class HttpResponse {
	import HttpResponse.DefaultEncoding

	private var status: Option[Int] = None
	private var header: Map[String, String] = Map()
	private var strBody: Option[String] = None
	private var bodyEncoding = DefaultEncoding

	/**
	 * Install HTTP status.
 	 * @param s Status code
	 * @return this
	 */
	def status(s: Int): HttpResponse = {
		status = Some(s)
		this
	}

	/**
	 * Adds header item.
	 * @param key Header key
	 * @param value Header value
	 * @return this
	 */
	def header(key: String, value: String): HttpResponse = {
		header += (key -> value)
		this
	}

	/**
	 * Adds multiple header items.
	 * @param map Header items to add
	 * @return this
	 */
	def header(map: Map[String, String]): HttpResponse = {
		header = this.header ++ map
		this
	}

	/**
	 * Sets HTTP response body as plain text.
	 * @param strBody HTTP response body
	 * @param encoding HTTP response body encoding
	 * @return
	 */
	def plain(strBody: String, encoding: String = DefaultEncoding) = {
		header("Content-Type", s"text/plain; charset=${encoding.toLowerCase}")
		body(strBody)
		this
	}

	/**
	 * Sets Server header item.
	 * @param name A name of a server
	 * @return this
	 */
	def server(name: String) = {
		header("Server", name)
		this
	}

	/**
	 * Sets Date header item to current Date.
	 * @return this
	 */
	def date = {
		header("Date", new Date().toString)
		this
	}

	/**
	 * Sets Date header item.
	 * @param date A date
	 * @return this
	 */
	def date(date: Date) = {
		header("Date", Option(date).getOrElse("").toString)
	}

	private def body(body: String, encoding: String = DefaultEncoding) = {
		strBody = Some(body)
		bodyEncoding = encoding
		this
	}

	override def toString =
		new StringBuilder()
			.append("*HTTP/1.1 ").append(status).append('\n')
			.append(s"*Header: $header").append('\n')
			.append(s"*Body: $strBody")
			.toString()

	/**
	 * Converts HttpResponse to a sequence of bytes.
	 * @return A sequence of bytes
	 */
	def build: Array[Byte] = {
		val buf = new ArrayBuffer[Byte] { override val initialSize = 1024 }
		val sb = new StringBuilder

		// grab status and status description
		val stat = status.getOrElse(0)
		val meaning = StatusMeaning(stat)

		// form http response start line
		sb.append(s"HTTP/1.1 $stat $meaning\r\n")
		header.map { case (param, value) => sb.append(s"$param: $value\r\n")}
		sb.append("\r\n")

		// convert response start line and header to bytes
		buf ++= str2Bytes(sb.toString(), US_ASCII)

		// convert response body to bytes
		buf ++= str2Bytes(strBody.getOrElse(""), bodyEncoding)

		buf.toArray
	}
}

object HttpResponse {
	val DefaultEncoding = UTF_8

	/**
	 * Sets response body to plain text and response status 200 OK.
	 * @param strBody Response body plain text
	 * @param encoding Body encoding
	 * @return HttpResponse
	 */
	def ok(strBody: String, encoding: String = DefaultEncoding) = {
		plainWithStatus(200, strBody, encoding)
	}

	/**
	 * Sets response body to plain text and response status 400 Not Found.
	 * @param strBody Response body plain text
	 * @param encoding Body encoding
	 * @return HttpResponse
	 */
	def notFound(strBody: String, encoding: String = DefaultEncoding) = {
		plainWithStatus(404, strBody, encoding)
	}

	/**
	 * Sets response body to plain text and response status 500 Internal Server Error.
	 * @param strBody Response body plain text
	 * @param encoding Body encoding
	 * @return HttpResponse
	 */
	def internalServerError(strBody: String, encoding: String = DefaultEncoding) = {
		plainWithStatus(500, strBody, encoding)
	}

	private def plainWithStatus(status: Int, strBody: String, encoding: String = DefaultEncoding) = {
		new HttpResponse().plain(strBody, encoding).status(status)
	}
}