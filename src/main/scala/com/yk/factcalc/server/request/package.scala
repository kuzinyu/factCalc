package com.yk.factcalc.server

import com.yk.factcalc._

import java.io.{BufferedInputStream, InputStream}
import java.net.{InetSocketAddress, Socket}

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

package object request {

	/**
	 * Translates a sequence of bytes to a map of parameters.
	 * @param bytes Bytes to translate
	 * @param encoding Encoding to be used for translation
	 * @param parSplit Parameters splitter
	 * @param kvSplit Key-value pair splitter
	 * @return A Map with parsed parameters (an empty map if no parameters available)
	 */
	def bytes2Params(bytes: Array[Byte], encoding: String, parSplit: Char = '&', kvSplit: Char = '=') = {
		str2Params(bytes2Str(bytes, encoding), parSplit, kvSplit)
	}

	/**
	 * Converts a sequence of characters to a map of parameters.
	 * @param str A string to convert
	 * @param parSplit Parameters splitter
	 * @param kvSplit Key-value pair splitter
	 * @return A Map with parsed parameters (an empty map if no parameters available)
	 */
	def str2Params(str: String, parSplit: Char = '&', kvSplit: Char = '=') = {
		val content = str.trim.split(parSplit)
		content.map(param => param.split(kvSplit.toString, -1))
					 .flatMap(kv => if (kv.length > 1) Some(kv(0).trim -> kv(1).trim) else None)
					 .toMap
	}

	object HTTPMessageIOUtils {

		val US_ASCII = "US-ASCII"
		val ISO_8859_1 = "ISO-8859-1"
		val UTF_8 = "UTF-8"

		/**
		 * Reads length bytes from the input stream.
		 * @param in An input stream to read bytes from
		 * @param length A number of bytes to be read from a stream
		 * @return Bytes read
		 */
		def read(in: InputStream, length: Int): Array[Byte] = {
			val buffer = new Array[Byte](length)

			// TODO return offset as well to compare with the length
			@tailrec
			def read(offset: Int): Array[Byte] = {
				offset match {
					case n if n < length =>
						val bytesRead = in.read(buffer, offset, length - offset)
						if (bytesRead == -1) buffer
						else read(offset + bytesRead)

					case _ => buffer
				}
			}

			// offset contains the total number of read bytes, should be equal to the length
			// TODO whether an exception should be thrown is a good question; compare with the length
			read(0)
		}

		sealed abstract class LineBreak
		case object EOF extends LineBreak
		case object EOL extends LineBreak

		/**
		 * Reads a sequence of bytes from an input stream until a line break or EOF is found.
		 * @param in A stream to read bytes from
		 * @return A sequence of bytes and line break
		 */
		def readLine(in: InputStream): (Array[Byte], LineBreak) = {
			val buf = new ArrayBuffer[Byte] { override val initialSize = 1024 }

			@tailrec
			def read: (Array[Byte], LineBreak) = {
				in.read() match {
					case -1   => (buf.toArray, EOF)
					case '\r' => read
					case '\n' => (buf.toArray, EOL)
					case byte => buf += byte.toByte; read
				}
			}

			read
		}

		/**
		 * This reads all available bytes from the input stream.
		 * @param in A stream to read bytes from
		 * @return A sequence of bytes read
		 */
		def readAvailable(in: InputStream): Array[Byte] = {
			val buf = new ArrayBuffer[Byte] { override val initialSize = 1024 }

			@tailrec
			def read: Array[Byte] = {
				in.read() match {
					case -1   => buf.toArray
					case byte => buf += byte.toByte; println(buf); read
				}
			}

			read
		}

		/**
		 * A simple holder for HTTP start line with parameters parsed from the URI.
		 * @param method HTTP method
		 * @param uri Request URI
		 * @param version HTTP version
		 * @param requestParams Request parameters
		 */
		case class StartLine(method: String, uri: String, version: String, requestParams: Map[String, String])

		/**
		 * Given an input stream, parse HTTP request start line.
		 * @param in An input stream to parse start line from
		 * @return HTTP start line and a line break
		 */
		def parseStartLine(in: InputStream): (Option[StartLine], LineBreak) = {
			// read request start line
			val (startLineBytes, break) = readLine(in)
			// split into method, uri, version
			val tokens = bytes2Str(startLineBytes, US_ASCII).trim.split("\\s+")
			// assume version is always available
			if (tokens.length < 2) (None, break)
			else {
				val method = tokens(0).toUpperCase
				val uri = tokens(1)
				val version = if (tokens.length > 2) tokens(2) else ""
				val requestParams = str2Params(uri.dropWhile(_ != '?').drop(1))

				(Some(StartLine(method, uri, version, requestParams)), break)
			}
		}

		/**
		 * Given an input stream, parse HTTP header section.
		 * @param in An input stream to parse a header from
		 * @return A map containing parsed header items (can be empty)
		 */
		def parseHeader(in: InputStream): Map[String, String] = {
			val header = immutable.Map.newBuilder[String, String]

			@tailrec
			def parse: Map[String, String] = {
				// read the header line by line
				readLine(in) match {
					case (Array(), _)   => header.result()
					case (lineBytes, _) =>
						// found non-empty line, split it into key-value pair
						val kvPair = bytes2Str(lineBytes, US_ASCII).trim.split(":", 2)
						// add to the result map only if header value was available
						if (kvPair.length > 1)
							header += kvPair(0).trim.toLowerCase -> kvPair(1).trim

						parse
				}
			}

			parse
		}

		/**
		 * Given an input stream, parse HTTP request body.
		 * @param in An input stream to parse a body from
		 * @param header A map of available header section items
		 * @param method HTTP method
		 * @return Either a map of form parameters or a payload byte sequence
		 */
		def parseContent(in: InputStream, header: Map[String, String], method: HttpMethod): Either[Map[String, String], Array[Byte]] = {
			def body2Params(encoding: String): Map[String, String] = {
				header.get("content-length") match {
					case Some(cl) =>
						val length = str2Int(cl, 0)
						bytes2Params(read(in, length), encoding)

					case None =>
						bytes2Params(readAvailable(in), encoding)
				}
			}

			(method, header.get("content-type")) match {
				// the request is either post or put with Content-Type specified
				case (Post | Put, Some(ct)) =>
					// if its form data, convert the message body to parameters map
					// taking encoding into account, otherwise just read what's available as plain bytes
					if (ct.indexOf("x-www-form-urlencoded") != -1) {
						val charsetAttr = "charset="
						val charsetStart = ct.indexOf(charsetAttr)
						val encoding = if (charsetStart != -1) ct.substring(charsetStart + charsetAttr.length)
													 else ISO_8859_1

						Left(body2Params(encoding))
					} else {
						Right(readAvailable(in))
					}

				// the request is either post or put, but with no Content-Type specified
				case (Post | Put, None) =>
					// simply read what's available in payload
					Right(readAvailable(in))

				case _ =>
					Left(Map.empty[String, String])
			}
		}

		/**
		 * Given a socket, produce a request object.
		 * @param socket A socket to request data from
		 * @return A request read from a socket
		 */
		def parseRequest(socket: Socket): HttpRequest = {
			// get remote socket address
			val socketAddress = socket.getRemoteSocketAddress.asInstanceOf[InetSocketAddress]

			val in = new BufferedInputStream(socket.getInputStream)

			// parse start line e.g. GET /resource HTTP/1.1
			val (startLine, _) = parseStartLine(in)
			startLine match {
				// parse GET and DELETE requests
				case Some(sl @ HttpMethod(method)) if method == Get || method == Delete =>
					HttpRequest(method, sl.uri, sl.version, parseHeader(in), sl.requestParams, socketAddress, Array.empty[Byte])

				// parse POST and PUT requests
				case Some(sl @ HttpMethod(method)) if method == Post || method == Put =>
					val header = parseHeader(in)
					val (requestParams, bytesContent) =
						parseContent(in, header, method) match {
							case Left(request) => (request, Array.empty[Byte])
							case Right(bytes)  => (Map.empty[String, String], bytes)
						}

					HttpRequest(method, sl.uri, sl.version, header, requestParams, socketAddress, bytesContent)

				// unknown verb
				case None =>
					HttpRequest()
			}
		}
	}
}
