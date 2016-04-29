package com.yk.factcalc.main

import com.yk.factcalc._
import com.yk.factcalc.cache.Memoizer
import com.yk.factcalc.logger.Logger
import com.yk.factcalc.server.resource.Resource
import com.yk.factcalc.server.response.HttpResponse

import scala.concurrent.{ExecutionContext, Future}

class CalcResource(cache: Memoizer[Int, BigInt])
                  (override implicit val ec: ExecutionContext, override implicit val logger: Logger) extends Resource {

	// localhost:80/calc?n=100
	get("/calc") { request =>
		Future {
			request.params.get("n") match {
				case Some(n) =>
					str2Int(n) match {
						case Some(intValue) =>
							val fact = cache(intValue)
							logger.info(s"FACT($n) = $fact")
							response.plain(fact.toString()).status(200)

						case None           =>
							response.plain("Please specify a valid integer number").status(400)
					}

				case None    =>
					response.plain("Integer parameter n is required").status(500)
			}
		}
	}

	// curl --data "s=hello" localhost:80/echo
	post("/echo") { request =>
		Future {
			request.params.get("s") match {
				case Some(s) => HttpResponse.ok(s)
				case None    => HttpResponse.internalServerError("No param s found, please specify one")
			}
		}
	}

	// localhost:80/systemShock
	get("/systemShock") { request =>
		Future.successful {
			HttpResponse.ok((1 / 0).toString)
		}
	}
}