package home.examples

trait Person {
  def fullName: String
  def age: Int
}

class SimplePerson(firstName: String, lastName: String) extends Person {
  require(!firstName.isEmpty, !lastName.isEmpty)

  var age: Int = 18

  override def fullName: String = s"$firstName $lastName"
}

class SecretAgent(firstName: String, lastName: String, inIncognito: Boolean)
    extends SimplePerson(firstName, lastName) {

  override def fullName: String = if (inIncognito) "Secret" else super.fullName
}

class SecretAgent2(firstName: String, lastName: String, inIncognito: Boolean) extends Person {
  require(!firstName.isEmpty, !lastName.isEmpty) //check in constructor duplicated

  var age: Int = 18 //field duplicated

  private def normalFullName: String = s"$firstName $lastName" //method duplicated

  def fullName: String = if (inIncognito) "Secret" else normalFullName
}

object Garbage extends App {

  val p1 = new SecretAgent("23", "4", true)
  val p2 = new SecretAgent2("23", "4", true)
  println(p1.age)
  println(p2.age)
  p1.age = 6
  p2.age = 8
  println(p1.age)
  println(p2.age)
}
