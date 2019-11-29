package home.examples.tenets

object Encapsulation2 {

  type Card = String

}

import home.examples.CardGame.Card

import scala.collection.immutable.Seq

object Encapsulation2OOP {

  trait Deck {
    def pop(): Option[Card]
    def isEmpty(): Boolean
  }

  trait Player {
    def deal(card: Card): Unit
    def discardOne(): Unit
    def showHand(): Seq[Card]
  }

  trait Game {
    def players(): Seq[Player]
    def deck(): Deck
  }

  def run(game: Game): Unit = {
    val deck = game.deck()
    val player = game.players()

    while (!deck.isEmpty()) {
      player.foreach { player ⇒
        val topCard = deck.pop()
        topCard.foreach(player.deal)
      }
    }
  }

  trait Table {
    def deal(): Unit
  }

  class TableImpl(deck: Deck, players: Seq[Player]) extends Table {

    private val currentPlayer: () ⇒ Player = {
      val x: Iterator[Int] = Iterator.continually(0 to players.size - 1).flatten
      () ⇒
        players(x.next())
    }

    override def deal(): Unit = {
      ???
//      deck.pop().foreach { card ⇒
//        players(currentPlayer()).deal(card)
//      }
    }
  }
}

object Encapsulation2FP {}
