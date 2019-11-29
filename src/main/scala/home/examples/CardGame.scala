package home.examples

import scala.collection.immutable._
import scala.collection.mutable.ListBuffer

//  sealed trait Suite
//  object Suite {
//    case object Spades extends Suite
//    case object Heart extends Suite
//    case object Clubs extends Suite
//    case object Diamonds extends Suite
//  }
//
//  sealed trait Rank
//  object Rank {
//    case object Spades extends Suite
//    case object Heart extends Suite
//    case object Clubs extends Suite
//    case object Diamonds extends Suite
//  }

object CardGame {

  type Suite = String
  type Rank = String

  val suites: Seq[Suite] = Seq("Spades", "Hearts", "Clubs", "Diamonds")
  val ranks: Seq[Rank] = Seq(
    "Two",
    "Three",
    "Four",
    "Five",
    "Six",
    "Seven",
    "Eight",
    "Nine",
    "Ten",
    "Jack",
    "Queen",
    "King",
    "Ace")

  final case class Card(rank: Rank, suite: Suite)

  trait Game {
    def run(cards: Seq[Card], players: Int): Seq[Card]
  }

}

import CardGame._

object CardGameFP {

  trait Deck {
    def pop(): (Deck, Option[Card])
    def isEmpty(): Boolean
  }

  trait Player {
    def deal(card: Card): Player
    def discardOne(): Player
    def showHand(): Seq[Card]
  }

  case class SimpleDeck(cards: Seq[Card]) extends Deck {

    override def pop(): (Deck, Option[Card]) =
      cards match {
        case topCard +: rest ⇒ (copy(rest), Some(topCard))
        case _ ⇒ (this, None)
      }

    override def isEmpty(): Boolean = cards.isEmpty
  }

  case class SimplePlayer(cards: Seq[Card]) extends Player {
    override def deal(card: Card): Player = copy(cards = cards :+ card)
    override def discardOne(): Player = copy(cards = cards.drop(1))
    override def showHand(): Seq[Card] = cards
  }

  class GameFP(players: Seq[Player], deck: Deck) extends Game {

    final case class State(players: Seq[Player], deck: Deck, winner: Option[Player])

    def run(): Option[Player] = {

      def dealACard(player: Player, deck: Deck): (Player, Deck) = {
        val (updatedDeck, card) = deck.pop()

        val updatedPlayer = card
          .map(player.deal)
          .getOrElse(player)

        (updatedPlayer, updatedDeck)
      }

      def dealXCards(cards: Int, player: Player, deck: Deck): (Player, Deck) = {
        (1 to cards).foldLeft((player, deck)) { (acc, i) ⇒
          val (player, deck) = acc
          dealACard(player, deck)
        }
      }

      //initially everyone receives 2 cards
      val (updatedDeck, updatedPlayers) = players.foldLeft((deck, Seq.empty[Player])) {
        (acc, player) ⇒
          val (deck, players) = acc
          val (updatedPlayer, updatedDeck) = dealXCards(2, player, deck)
          (updatedDeck, players :+ updatedPlayer)
      }

      def runIt(state: State): State =
        if (state.deck.isEmpty() || state.winner.nonEmpty)
          state
        else {
          runIt(oneRound(state))
        }

      val endState = runIt(State(deck = updatedDeck, players = updatedPlayers, winner = None))
      endState.winner

    }

    def oneRound(state: State): State = {

      //all the players get dealt a card

      val init: (Deck, Seq[Player]) = (state.deck, Seq.empty[Player])

      val (updatedDeck, updatedPlayers) = state.players.foldLeft(init) { (acc, player) ⇒
        val (deck, players) = acc
        val (newDeck, card) = deck.pop()

        val updatedPlayer = card
          .map { c ⇒
            player.deal(c).discardOne()
          }
          .getOrElse(player)

        (newDeck, players :+ updatedPlayer)
      }

      val winner = updatedPlayers.find { player ⇒
        player.showHand() == Card("Ace", "Hearts")
      }
      State(deck = updatedDeck, players = updatedPlayers, winner = winner)

    }

    override def run(cards: Seq[Card], players: Int): Seq[Card] = {
      ???
    }
  }
}

object CardGameOOP {

  trait Deck {
    def pop(): Option[Card]
    def isEmpty(): Boolean
  }

  trait Player {
    def deal(card: Card): Unit
    def discardOne(): Unit
    def showHand(): Seq[Card]
  }

  class GameOO(players: Seq[Player], deck: Deck) extends Game {

    def run(): Option[Player] = {

      //initially everyone receves 2 cards
      players.foreach { player ⇒
        (1 to 2).foreach { _ ⇒
          deck.pop().foreach(player.deal)
        }

      }

      var winner: Option[Player] = None
      while (!deck.isEmpty() && winner.isEmpty) {
        winner = oneRound()
      }
      winner
    }

    def oneRound(): Option[Player] = {
      //all the players get dealt a card
      players.foreach { player ⇒
        deck.pop().foreach { card ⇒
          player.deal(card)
          player.discardOne()
        }
      }

      //everyone show their hands, find the one with the winning combination
      players.find { player ⇒
        player.showHand() == Card("Ace", "Hearts")
      }
    }

    override def run(cards: Seq[Card], players: Int): Seq[Card] = {
      ???
    }
  }

  class SimpleDeck extends Deck {

    private val cards: ListBuffer[Card] = {
      val cs = for { rank ← ranks; suite ← suites } yield Card(rank, suite)
      ListBuffer(cs: _*)
    }

    override def pop(): Option[Card] = {
      val topCard = cards.headOption
      cards.drop(1)
      topCard
    }

    override def isEmpty(): Boolean = cards.isEmpty
  }

  class SimplePlayer extends Player {

    private val cards: ListBuffer[Card] = ListBuffer.empty

    override def deal(card: Card): Unit = {
      cards.append(card)
    }

    override def showHand(): Seq[Card] = cards.toList

    override def discardOne(): Unit = {
      cards.remove(1)
    }
  }

  class CheatingPlayer(cardInSleeve: Card) extends Player {

    private val cards: ListBuffer[Card] = ListBuffer.empty

    override def deal(card: Card): Unit = {
      cards.append(card)
    }

    override def discardOne(): Unit = {
      cards.remove(1)
    }

    override def showHand(): Seq[Card] = Seq(cardInSleeve)
  }
}
