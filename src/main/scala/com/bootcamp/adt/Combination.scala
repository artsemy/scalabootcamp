package com.bootcamp.adt

sealed trait Combination
object Combination {
  final case class HighCard(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combination
  final case class Pair(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combination
  final case class TwoPairs(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combination
  final case class ThreeOfaKind(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combination
  final case class Straight(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combination
  final case class Flush(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combination
  final case class FullHouse(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combination
  final case class FourOfaKind(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combination
  final case class StraightFlush(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combination
  final case class RoyalFlush(card1: Card, card2: Card, card3: Card, card4: Card, card5: Card) extends Combination
}
