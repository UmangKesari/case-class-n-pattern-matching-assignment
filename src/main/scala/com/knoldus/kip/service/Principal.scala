package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models._


trait Principal {

  def findOutIfCSE(id: Int): CoursePerformance = {

    val coursePerformance: Option[CoursePerformance] = RamDatabase.getById(id)

    val course: Option[String] = coursePerformance.map(_.course.name).getOrElse("None")

    course match {

      case Some("CSE") => coursePerformance.get

      case None => throw new Exception("no CSE course with this id")

    }
  }

  def findOutIfAnyCourse(id: Int, courseName: String): CoursePerformance = {

    val receivedCoursePerformance = RamDatabase.getById(id)
    val receiveCourseName = receivedCoursePerformance.map(_.course.name).getOrElse("None")
    receiveCourseName match {
      case courseName => receivedCoursePerformance.get
      case _ => throw new Exception
    }
  }

  def expression(mod: Any): String = {
    mod match {
      case x: Student => "Shut up "
      case x: Subject => "Hmmm .... "  /* change */
      case x: Scoreboard => "aha "
      case _ => "!!! ???"
    }
  }
/*
    def checkScoreboard(scoreboard: Scoreboard): List[String] = {


    }
*/
  def expressionRevisited: PartialFunction[ModelIdentifier, String] = {
    case x: Student => "Shut up "
    case x: Scoreboard => "Hmmm .... "
    case x: Subject => "aha "
    case _ => "!!! ???"
  }

}
