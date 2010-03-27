package jp.gr.java_conf.mizu.pegex

case class MatchResult(result: Option[String], group: Map[Symbol, String])