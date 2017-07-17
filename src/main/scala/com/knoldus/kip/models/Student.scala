package com.knoldus.kip.models

case class Subject(id : Int, name : String, maxMarks : Int, obtainedMarks : Int ) extends ModelIdentifier

case class Student(id : Int, firstName : String,middleName : Option[String],lastName : String,
                   rollNumber : Int, age : Option[Int], Gender : Char, enrollmentNumber : Int,
                   address : Option[String]) extends ModelIdentifier {

  def getAddress: String = address.fold("N/A"){_}
  def getMiddleName: String = middleName.map(_.split(" ").head).getOrElse("N/A")


}

case class Scoreboard(id : Int, student : Student, subjects : List[Subject], total : Int,
                      percentage : Double, grade : String ) extends ModelIdentifier
{
  def getSubjectWithHighestMarks : List[Subject] = {
      val listOfMarks = subjects.map(_.obtainedMarks)
      val highestScore = listOfMarks.max
      subjects.filter(_.obtainedMarks == highestScore)

    }

  def getSubjectWithLowestMarks : List[Subject] = {
    val listOfMarks = subjects.map(_.obtainedMarks)
    val lowestScore = listOfMarks.min
    subjects.filter(_.obtainedMarks == lowestScore)

  }
}


object Scoreboard {

  def apply(student: Student, subjects: List[Subject]): Scoreboard = {
    val totalMarks= subjects.map(_.obtainedMarks).sum
    val percentage = totalMarks / subjects.map(_.maxMarks).sum

    val grade = if(percentage >= 95)
    {
      "A+"
    }
    else if(percentage >= 90)
    {
      "A"
    }
    else if(percentage >= 85)
    {
      "B+"
    }
    else if(percentage >= 80)
    {
      "B"
    }
    else if(percentage >= 70)
    {
      "C+"
    }
    else if(percentage >= 60)
    {
      "C"
    }
    else if(percentage >= 50)
    {
      "D+"
    }
    else if(percentage >= 40)
    {
      "D"
    }
    else
    {
      "F"
    }
    new Scoreboard(student.id, student, subjects, totalMarks,
      percentage, grade)
  }

  def checkScoreboard(scorecards: List[Scoreboard]): List[String] = {
    for{
      scoreboard <- scorecards
      highestScore <- scoreboard.getSubjectWithHighestMarks
    } yield {scoreboard.student.firstName + " " + highestScore.name + " " + highestScore.obtainedMarks}
  }
}

case class Course(id : Int, name : String, category : String,
                  subjects : List[Subject]) extends ModelIdentifier

case class CoursePerformance(id : Int, year : Int, course : String,
                             scoreCards : Scoreboard) extends ModelIdentifier