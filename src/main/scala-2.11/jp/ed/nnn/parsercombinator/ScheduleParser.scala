package jp.ed.nnn.parsercombinator

import scala.util.parsing.combinator._


object ScheduleParser extends JavaTokenParsers {
  def titleMatchSchedules: Parser[(String, List[(List[Int], String)])] =
    "(" ~ "title-match-schedules" ~ rep(schedule) ~ ")" ^^ {
      t => (t._1._1._2, t._1._2)
    }

  def schedule: Parser[(List[Int], String)] =
    "(" ~ date ~ "," ~ string ~ ")" ^^ {
      t => (t._1._1._1._2, t._1._2)
    }

  def date: Parser[List[Int]] =
    digits ~ "年" ~ digits ~ "月" ~ digits ~ "日" ^^ {
      t => List(t._1._1._1._1._1.toInt, t._1._1._1._2.toInt, t._1._2.toInt)
    }

  def digits: Parser[String] = "[0-9]+".r

  def string: Parser[String] = stringLiteral ^^ { s => s.substring(1, s.length - 1) }

  def apply(input: String): (String, List[(List[Int], String)]) = parseAll(titleMatchSchedules, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }
}
