package com
package github
package kmizu
package pegex

import scala.collection.{DefaultMap, Map}

case class MatchResult(result: Option[String], group: Map[Symbol, String]) extends DefaultMap[Symbol, String] {
  def iterator: Iterator[(Symbol, String)] = group.iterator
  def get(key: Symbol): Option[String] = group.get(key)
}