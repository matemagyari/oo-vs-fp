package home.examples.tenets

import java.time.LocalDate

object Inheritance {}

object InheritanceOOP {

  abstract class Employee(name: String, startDate: LocalDate)

  class Manager(name: String, startDate: LocalDate, department: String)
      extends Employee(name, startDate)
}

object InheritanceFP {

  case class Employee(name: String, startDate: LocalDate)

  class Manager(name: String, startDate: LocalDate, department: String)
      extends Employee(name, startDate)
}
