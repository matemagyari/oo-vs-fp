package home.examples.tenets

object Encapsulation {
  //  case class TaskDetails(name: String, description: String, assignee: String, state: State)
  case class TaskDetails(name: String, description: String)

}

import java.util.concurrent.atomic.AtomicInteger

import Encapsulation._
import home.examples.tenets.EncapsulationOOP.Task

object EncapsulationOOP {

  class Task(val id: Int, private val details: TaskDetails) {
    private var name: String = details.name
    private var description: String = details.description

    def setName(name: String): Unit = {
      this.name = name
    }

    def setDescription(description: String): Unit = {
      this.description = description
    }
  }

  trait Board {
    def newTask(details: TaskDetails): Task
    def task(id: Int): Option[Task]
    def deleteTask(id: Int): Unit
  }

  class BoardImpl extends Board {
    private val tasks = scala.collection.mutable.Map.empty[Int, Task]

    private val idGenerator = new AtomicInteger()

    override def newTask(details: TaskDetails): Task = {
      val task = new Task(id = idGenerator.incrementAndGet(), details = details)
      tasks.put(task.id, task)
      task
    }

    override def task(id: Int): Option[Task] = tasks.get(id)

    override def deleteTask(id: Int): Unit = {
      tasks.remove(id)
    }
  }
}

object EncapsulationFP {

  case class Task(id: Int, details: TaskDetails)

  trait Board {
    def newTask(details: TaskDetails): Task
    def task(id: Int): Option[Task]
    def updateTask(task: Task): Unit
    def deleteTask(id: Int): Unit
  }

  class BoardImpl extends Board {

    private val tasks = scala.collection.mutable.Map.empty[Int, Task]

    private val idGenerator = new AtomicInteger()

    override def newTask(details: TaskDetails): Task = {
      val task = Task(id = idGenerator.incrementAndGet(), details = details)
      tasks.put(task.id, task)
      task
    }

    override def task(id: Int): Option[Task] = tasks.get(id)

    override def deleteTask(id: Int): Unit = {
      tasks.remove(id)
    }

    override def updateTask(task: Task): Unit = {
      tasks.put(task.id, task)
    }
  }
}
