package home.examples.chat

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Chat {

  type UserName = String
  type Message = String
  type IdentifiedMessage = (UserName, Message)

  trait ChatProgram {

    def users(): Seq[UserName]
    def send(from: UserName, to: UserName, message: Message): Unit
  }

}

import Chat._

object NaiveChat {

  class NaiveChatProgram extends ChatProgram {

    type Session = Set[UserName]

    private val userList: ListBuffer[UserName] = ListBuffer.empty

    private val sessions: TrieMap[Session, ListBuffer[IdentifiedMessage]] = TrieMap.empty

    override def users(): Seq[String] = userList.toList

    override def send(from: UserName, to: UserName, message: Message): Unit = {
      sessions
        .putIfAbsent(Set(from, to), ListBuffer.empty[IdentifiedMessage])
        .foreach { messages ⇒
          messages.append((from, message))
        }
    }
  }

}

object OOChat {

  class Session(val users: Set[UserName]) {

    private val messages: ListBuffer[IdentifiedMessage] = ListBuffer.empty

    def messageArrived(user: UserName, message: Message): Unit = {
      require(users.contains(user))
      messages.append((user, message))
    }

  }

  class OOChatProgram extends ChatProgram {

    private val userList: ListBuffer[UserName] = ListBuffer.empty

    private val sessions: ListBuffer[Session] = ListBuffer.empty

    override def users(): Seq[String] = userList.toList

    override def send(from: UserName, to: UserName, message: Message): Unit = {
      sessions
        .find(_.users == Set(from, to))
        .orElse {
          val session = new Session(Set(from, to))
          sessions.append(session)
          Some(session)
        }
        .foreach { session ⇒
          session.messageArrived(from, message)
        }
    }
  }

}

object FPChat {

  final case class Session(users: Set[UserName], messages: Seq[IdentifiedMessage])

  class FPChatProgram extends ChatProgram {

    private val userList: ListBuffer[UserName] = ListBuffer.empty

    private val sessions: TrieMap[Set[UserName], Session] = TrieMap.empty

    override def users(): Seq[String] = userList.toList

    override def send(from: UserName, to: UserName, message: Message): Unit = {
      val users = Set(from, to)
      val session = sessions.getOrElse(users, Session(users, Seq.empty))
      val updatedSession = session.copy(messages = session.messages :+ (from, message))
      sessions.put(users, session)
    }
  }

}
