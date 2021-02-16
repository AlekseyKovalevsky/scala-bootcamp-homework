package akovalevsky.scalabootcamp.homework.adt

import akovalevsky.scalabootcamp.homework.adt.PokerAdt.Card.Card
import akovalevsky.scalabootcamp.homework.adt.PokerAdt.Rank.RankParser
import akovalevsky.scalabootcamp.homework.adt.PokerAdt.Suit.SuitParser

import scala.collection.SortedSet

object PokerAdt {

  sealed trait Rank

  object Rank {

    implicit class RankParser(c: Char) {
      private val rankSymbols = Map(
        'a' -> Ace,
        'k' -> King,
        'q' -> Queen,
        'j' -> Jack,
        't' -> Ten,
        '9' -> Nine,
        '8' -> Eight,
        '7' -> Seven,
        '6' -> Six,
        '5' -> Five,
        '4' -> Four,
        '3' -> Three,
        '2' -> Two
      )

      def toRank: Option[Rank] = rankSymbols.get(this.c.toLower)
    }

    final case object Ace extends Rank

    final case object King extends Rank

    final case object Queen extends Rank

    final case object Jack extends Rank

    final case object Ten extends Rank

    final case object Nine extends Rank

    final case object Eight extends Rank

    final case object Seven extends Rank

    final case object Six extends Rank

    final case object Five extends Rank

    final case object Four extends Rank

    final case object Three extends Rank

    final case object Two extends Rank

  }

  sealed trait Suit

  object Suit {

    implicit class SuitParser(c: Char) {
      private val suitSymbols = Map(
        'd' -> Diamonds,
        'c' -> Clubs,
        'h' -> Hearts,
        's' -> Spades
      )

      def toSuit: Option[Suit] = suitSymbols.get(this.c.toLower)
    }

    final case object Diamonds extends Suit

    final case object Clubs extends Suit

    final case object Hearts extends Suit

    final case object Spades extends Suit

  }

  object Card {

    implicit class CardParser(str: String) {
      def toCard: Option[Card] =
        if (str.length == 2)
          for {
            rank <- str(0).toRank
            suit <- str(1).toSuit
          } yield Card(rank, suit)
        else
          None
    }

    final case class Card(rank: Rank, suit: Suit)

  }

  case class Board private(cards: List[Card]) extends AnyVal

  object Board {
    def apply(cards: List[Card]): Option[Board] = {
      if (cards.length == 5)
        Some(new Board(cards))
      else
        None
    }
  }

  sealed trait Combination

  object Combination {

    final object StraightFlush extends Combination

    final object FourOfAKind extends Combination

    final object FullHouse extends Combination

    final object Flush extends Combination

    final object Straight extends Combination

    final object ThreeOfAKind extends Combination

    final object TwoPair extends Combination

    final object Pair extends Combination

    final object HighCard extends Combination

  }

  abstract case class Hand(cards: List[Card])

  final class OmahaPokerHand private(cards: List[Card]) extends Hand(cards)

  object OmahaPokerHand {
    def apply(cards: List[Card]): Option[OmahaPokerHand] =
      if (cards.length == 4)
        Some(new OmahaPokerHand(cards))
      else
        None

  }

  final class TexasPokerHand private(cards: List[Card]) extends Hand(cards)

  object TexasPokerHand {
    def apply(cards: List[Card]): Option[TexasPokerHand] =
      if (cards.length == 2)
        Some(new TexasPokerHand(cards))
      else
        None

  }

  final case class TestCase[T <: Hand](board: Board, hands: List[T])

  final case class Split[T <: Hand](hands: List[T]) extends AnyVal

  final case class TestResult[T <: Hand](board: Board, splits: SortedSet[Split[T]])

}
