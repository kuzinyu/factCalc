package com.yk.factcalc.main

import com.yk.factcalc.cache.Memoizer
import com.yk.factcalc.logger.Logger
import com.yk.factcalc.server.Server
import com.yk.factcalc._

import java.util.concurrent.Executors

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

object FactCalc {
	val DefaultPort = 80
	val NThreads = 100

	// init cache
	val cache = new Memoizer[Int, BigInt] (n => {
		@tailrec
		def go(n: Int, result: BigInt): BigInt = {
			n match {
				case m if m <= 1 => result
				case k           => go(n - 1, result * n)
			}
		}

		go(n, 1)
	})

	// prepare logger and execution context
	implicit val logger = Logger("FactCalc logger")
	implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(NThreads))

	def main(args: Array[String]) = {
		// parse port to listen connections on
		val port = args match {
			case Array(p @ _, _*) => str2Int(p, DefaultPort)
			case Array()          => DefaultPort
		}

		object httpServer extends Server("FactCalc 0.1", port, new CalcResource(cache))

		// start logger
		logger.start()

		// start server
		httpServer.start()

		// as simple as that
		httpServer.join()
	}
}
