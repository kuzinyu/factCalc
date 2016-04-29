package com.yk.factcalc.server.resource

import scala.util.matching.Regex

case class PathRegex(regex: Regex) {
	def apply(path: String) = regex.findFirstMatchIn(path)
}

object PathRegexer {
	// TODO The simplest path regex so far, add support for named parameters, wildcards, regexes
	def apply(path: String) = {
		val p = path.indexOf('?') match {
			case -1 => path
			case n  => path.substring(0, n)
		}

		PathRegex(s"^$p$$".r)
	}
}
