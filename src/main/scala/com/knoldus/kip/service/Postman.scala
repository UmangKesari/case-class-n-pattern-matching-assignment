package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{CoursePerformance,Scoreboard}


trait Postman {

  def getTheFirstAddressOfFirstYearPerformance(id: Int) : CoursePerformance = {
    RamDatabase.getById(id).flatMap(_.scoreCards.headOption.map(_.student.getAddress)).getOrElse("No Course Performance recieved")

  }

}
