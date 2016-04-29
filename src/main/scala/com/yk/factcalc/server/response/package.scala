package com.yk.factcalc.server

package object response {
	val StatusMeaning = Map(
		200 -> "OK",
		400 -> "Bad Request",
		404 -> "Not Found",
		500 -> "Internal Server Error",
		501 -> "Not Implemented"
	).withDefaultValue("Unknown Status")
}