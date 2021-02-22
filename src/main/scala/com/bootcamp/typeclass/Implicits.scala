package com.bootcamp.typeclass

object Implicits {}

object TypeclassTask extends App {

  // Why am I not a Typeclass?
  // TODO: Rework me so I am a typeclass
  trait HashCode[T] {
    def hash(entity: T): Int
  }

  object HashCode {
    // TODO: Implement me a summoner
    def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
  }

  implicit class HashCodeSyntax[A](x: A) {
    // TODO: Implement syntax so I can "abc".hash
    def hash(implicit i: HashCode[A]): Int = {i.hash(x)}
  }

  // TODO: make an instance for String
  // TODO: write "abc".hash to check everything

  implicit val HashString: HashCode[String] = str => str.length

  println("abc".hash)
  println("abcd".hash)
  println("123456789".hash)

}

object Task1 {
  final case class Money(amount: BigDecimal)

  // TODO: create Ordering instance for Money
  implicit val moneyOrdering: Ordering[Money] = (x: Money, y: Money) => x.amount compare y.amount
}
