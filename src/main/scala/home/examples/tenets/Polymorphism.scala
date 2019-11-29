package home.examples.tenets

object Polymorphism {

  trait NameSayer {
    def name(): String
  }

}
import home.examples.tenets.Polymorphism._
object PolymorphismOOP {

  case class Animal(species: String) extends NameSayer {
    override def name(): String = species
  }

  case class Human(humanName: String) extends NameSayer {
    override def name(): String = humanName
  }

  case class IntWrapper(i: Int) extends NameSayer {
    override def name(): String = i.toString
  }
}

object PolymorphismFP extends App {

  case class Animal(species: String)
  case class Human(humanName: String)

  implicit class AnimalNameSayer(a: Animal) extends NameSayer {
    override def name(): String = a.species
  }
  implicit class HumanNameSayer(h: Human) extends NameSayer {
    override def name(): String = h.humanName
  }
  implicit class IntNameSayer(i: Int) extends NameSayer {
    override def name(): String = i.toString
  }

  implicit def toNameSayer(s: String): NameSayer = new NameSayer {
    override def name(): String = s
  }

  Animal("Lion").name()
  Human("Joe").name()
  12.name()
  "sing".name()
}
