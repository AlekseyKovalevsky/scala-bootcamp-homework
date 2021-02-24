package akovalevsky.scalabootcamp.homework

object Typeclasses {

  object TypeclassTask {

    trait HashCode[A] {
      def hash(x: A): Int
    }

    object HashCode {
      def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
    }

    implicit class HashCodeSyntax[A](x: A) {
      def hash(implicit hashCode: HashCode[A]): Int = hashCode.hash(x)
    }

    object HashCodeImpl {
      val hashCodeForString: HashCode[String] = _.foldLeft((0, 0)) {
        case ((hash, currPos), currChar) => (hash + currChar.toInt * math.pow(31, currPos).toInt, currPos + 1)
      }._1

      val hashCodeForInt: HashCode[Int] = (x: Int) => x
    }

    implicit val hashCodeForString: HashCode[String] = HashCodeImpl.hashCodeForString
    implicit val hashCodeForInt: HashCode[Int] = HashCodeImpl.hashCodeForInt

    HashCode[String].hash("abc")
    HashCode[Int].hash(12)
    "abc".hash
    12.hash
  }

  object Task1 {

    final case class Money(amount: BigDecimal)

    implicit val orderingForMoney: Ordering[Money] = (x: Money, y: Money) => x.amount.compareTo(y.amount)
  }

  object Task2 {

    trait Show[T] {
      def show(entity: T): String
    }

    final case class User(id: String, name: String)

    implicit val showForUser: Show[User] = x => s"User(id: ${x.id}, name: ${x.name}"

    implicit class ShowSyntax[T](x: T) {
      def show(implicit show: Show[T]): String = show.show(x)
    }

    User("1", "Vasyl").show

  }

  object Task3 {
    type Error = String

    trait Parse[T] {
      def parse(entity: String): Either[Error, T]
    }

    final case class User(id: String, name: String)

    // Assuming here that CSV-string doesn't have quoted values. Just values separated by commas.
    implicit val parseForUser: Parse[User] = str => str.split(",").toList match {
      case id :: name :: Nil => Right(User(id, name))
      case _ => Left("Invalid string")
    }

    implicit class ParseSyntax(x: String) {
      def parse[A](implicit parser: Parse[A]): Either[Error, A] = parser.parse(x)
    }

    "Valhalla".parse[User]
    "1,Vasyl".parse[User]
  }

  object Task4 {

    trait Equality[A] {
      def equals(x: A, y: A): Boolean
    }

    implicit class EqualitySyntax[A](x: A) {
      def ===(y: A)(implicit eqComparer: Equality[A]): Boolean = eqComparer.equals(x, y)

      def !==(y: A)(implicit eqComparer: Equality[A]): Boolean = !(x === y)
    }

    final case class User(id: String, name: String)

    implicit val equalityForUser: Equality[User] = (x, y) => x.id == y.id && x.name == y.name

    User("1", "Vasyl") === User("1", "Vasyl") // true
    User("1", "Vasily") === User("1", "Vasyl") // false
    User("1", "Vasily") !== User("1", "Vasyl") // true
    // doesn't compile
    // User("1", "Vasyl") === "1,Vasyl"
  }

  object AdvancedHomework {

    trait FlatMap[F[_]] {
      def flatMap[A, B](x: F[A])(f: A => F[B]): F[B]
    }

    implicit class FlatMapSyntax[F[_], A](x: F[A]) {
      def flatMapCustom[B](f: A => F[B])(implicit mapper: FlatMap[F]): F[B] = mapper.flatMap(x)(f)
    }

    implicit val flatMapForList: FlatMap[List] = new FlatMap[List] {
      override def flatMap[A, B](x: List[A])(f: A => List[B]): List[B] = x.flatMap(f)
    }

    implicit val flatMapForOption: FlatMap[Option] = new FlatMap[Option] {
      override def flatMap[A, B](x: Option[A])(f: A => Option[B]): Option[B] = x match {
        case Some(value) => f(value)
        case _ => None
      }
    }
  }

}
