package com.yk.factcalc

import com.yk.factcalc.server.request.HttpRequest
import com.yk.factcalc.server.response.HttpResponse

import scala.concurrent.Future

package object server {
	type Callback = HttpRequest => Future[HttpResponse]
}
