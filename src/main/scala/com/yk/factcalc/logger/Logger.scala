package com.yk.factcalc.logger

import java.util.Date
import java.util.concurrent.LinkedBlockingQueue

import scala.annotation.tailrec

/**
 * A logging thread.
 * @param name A name of the logging thread
 */
class Logger(name: String) extends Thread {
	import com.yk.factcalc.logger.Logger.{Level, Info, Warn, Error}

	val messages = new LinkedBlockingQueue[String]

	setDaemon(true)
	setName(name)

	override def run() = {
		@tailrec
		def take(): Unit = {
			println(messages.take())
			take()
		}

		take()
	}

	/**
	 * Logs with INFO level.
	 * @param message A message to log
	 * @return No interesting value
	 */
	def info(message: String) = log(Info, message)

	/**
	 * Logs with WARN level.
	 * @param message A message to log
	 * @return No interesting value
	 */
	def warn(message: String) = log(Warn, message)

	/**
	 * Logs with ERROR level.
	 * @param t An error represented as a Throwable
	 * @param message A message to log
	 * @return No interesting value
	 */
	def error(t: Throwable, message: String) = log(Error, s"$message\n$t\n${t.getStackTraceString}")

	/**
	 * Logs with some log level.
	 * @param logLevel A log level
	 * @param message A message to log
	 * @return No interesting value
	 */
	def log(logLevel: Level, message: String) = {
		messages.offer(logMessage(logLevel, message))
	}

	private def logMessage(logLevel: Level, message: String) = {
		s"[${new Date()}] $logLevel $message"
	}
}

object Logger {
	def apply(name: String) = new Logger(name)

	sealed abstract class Level(name: String) {
		override def toString = name
	}

	case object Info extends Level("INFO")
	case object Warn extends Level("WARN")
	case object Error extends Level("ERROR")
}