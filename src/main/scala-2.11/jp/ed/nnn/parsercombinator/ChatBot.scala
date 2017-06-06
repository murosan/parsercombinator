package jp.ed.nnn.parsercombinator

import java.time.{LocalDateTime, ZoneId}

import scala.io.Source
import scala.util.Random
import scala.util.matching.Regex

case class ChatBot(commands: List[Command])

sealed trait Command {
  def exec(input :String): Boolean
}

case class ReplyCommand(regex: Regex, replays: List[String]) extends Command {
  override def exec(input :String): Boolean = {
    regex.findFirstIn(input) match {
      case Some(_) =>
        println(Random.shuffle(replays).head)
        true
      case None => false
    }
  }
}

case class TimeCommand(regex: Regex, start: Int, end: Int, zone: String, replays: List[String])
  extends Command {
  override def exec(input: String): Boolean = {
    val now = LocalDateTime.now().atZone(ZoneId.of(zone))
    val isInTime = start <= now.getHour && now.getHour <= end
    regex.findFirstIn(input) match {
      case Some(_) if isInTime =>
        println(Random.shuffle(replays).head)
        true
      case _ => false
    }
  }
}

case class TitleMatchScheduleCommand(regex: Regex, replies: List[String]) extends Command {
  override def exec(input: String): Boolean = {
    val nextMatch: (Boolean, String) = hasNextMatch
    regex.findFirstIn(input) match {
      case Some(_) if nextMatch._1 =>
        println(s"次のタイトル戦は、${nextMatch._2}だね。")
        println(Random.shuffle(replies).head)
        true
      case _ => false
    }
  }

  def hasNextMatch: (Boolean, String) = {
    val text = Source.fromFile("./title-match-schedules.txt").mkString
    val schedules: (String, List[(List[Int], String)]) = ScheduleParser(text)
    /* (title-match-schedules,
     List(
     (List(2017, 5, 26),"名人戦第5局1日目"),(List(5, 27),"名人戦第5局2日目"),
     (List(2017, 6, 5),"名人戦第6局1日目"),(List(6, 6),"名人戦第6局2日目"),
     (List(2017, 6, 21),"名人戦第7局1日目"),(List(6, 22),"名人戦第7局2日目"))
     )*/
    val now = LocalDateTime.now().atZone(ZoneId.of("Asia/Tokyo"))
    val date = (now.getYear, now.getMonthValue, now.getDayOfMonth)
    def hasNextMatchRec(scheduleList: List[(List[Int], String)]): (Boolean, String) = {
      if (scheduleList.nonEmpty) {
        scheduleList.head._1.head compare date._1 match {
          case 1 => (true, formatSchedule(scheduleList.head))
          case 0 => scheduleList.head._1(1) compare date._2 match {
            case 1 => (true, formatSchedule(scheduleList.head))
            case 0 => scheduleList.head._1(2) compare date._3 match {
              case 1 | 0 => (true, formatSchedule(scheduleList.head))
              case -1 => hasNextMatchRec(scheduleList.tail)
            }
            case -1 => hasNextMatchRec(scheduleList.tail)
          }
          case -1 => hasNextMatchRec(scheduleList.tail)
        }
      } else {
        (false, "")
      }

    }

    hasNextMatchRec(schedules._2)
  }

  def formatSchedule(list: (List[Int], String)): String = {
    s"　${list._1.head}年${list._1(1)}月${list._1(2)}日: ${list._2}　"
  }
}
