package com.yk.factcalc.cache

import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise, Future}
import scala.collection.convert.decorateAsScala._
import scala.util.Try

/**
 * This can memorize results of a computation. An idea taken from the Java Concurrency In Practice book.
 * @param computer A function which computes a result to be remembered
 * @tparam K A type of an argument of the function
 * @tparam V A return type
 */
class Memoizer[K, V](computer: K => V) {
	private val cache: collection.concurrent.Map[K, Future[V]] = new ConcurrentHashMap[K, Future[V]]().asScala

	def apply(k: K): V = {
		cache.get(k) match {
			case Some(future) => Await.result(future, Duration.Inf)
			case None         =>
				val p = Promise[V]()
				cache.putIfAbsent(k, p.future) match {
					case Some(future) => Await.result(future, Duration.Inf)
					case None         =>
						p.complete(Try(computer(k)))
						Await.result(p.future, Duration.Inf)
				}
		}
	}
}