import cats._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import io.circe.generic.auto._
import io.circe.parser.decode
import magnolia._
import shapeless.<:!<
import simulacrum.typeclass

import scala.language.experimental.macros
import scala.language.higherKinds

object Application {
  type NotString[A] = A <:!< String
  type NotCaseClass[A] = A <:!< Product

  final case class Address(city: String, zip: Int, street: String)
  final case class User(id: String, name: String, address: Address)

  @typeclass
  /* Effect of HTTP requests */
  trait HTTP[F[_]] {
    def get: F[String]
    def post(user: User): F[Unit]
  }

  @typeclass
  /* Effect of printing to the console */
  trait Console[F[_]] {
    def out(user: User): F[Unit]
  }

  /*Typeclass for upper/lower case converting*/
  trait CaseConverter[A] {
    def toUpper(a: A): A
    def toLower(a: A): A
  }

  /**
    * Logic for producing general instances of the above typeclass,
    * so it's able to work with any case class hierarchies and sealed traits
    * failing in compile time if unable to generate a typeclass instance
    * */
  object GenericCaseConverter {
    type Typeclass[T] = CaseConverter[T]

    def combine[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] =
      new CaseConverter[T] {
        override def toUpper(a: T): T =
          caseClass.construct(p => p.typeclass.toUpper(p.dereference(a)))
        override def toLower(a: T): T =
          caseClass.construct(p => p.typeclass.toLower(p.dereference(a)))
      }

    def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
      new CaseConverter[T] {
        override def toUpper(a: T): T =
          sealedTrait.dispatch(a)(t => t.typeclass.toUpper(t.cast(a)))
        override def toLower(a: T): T =
          sealedTrait.dispatch(a)(t => t.typeclass.toLower(t.cast(a)))
      }

    implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

    implicit val string: CaseConverter[String] = new CaseConverter[String] {
      override def toUpper(a: String): String = a.toUpperCase
      override def toLower(a: String): String = a.toLowerCase
    }

    implicit def default[T: NotString: NotCaseClass]: CaseConverter[T] =
      new CaseConverter[T] {
        override def toUpper(a: T): T = a
        override def toLower(a: T): T = a
      }
  }

  import GenericCaseConverter._

  /*Our program written in tagless final style*/
  def program[F[_]: Monad: HTTP: Console]: F[ExitCode] =
    for {
      json <- HTTP[F].get
      exitCode <- decode[User](json).fold(_ => ExitCode.Error.pure[F], { user =>
        Console[F].out(gen[User].toLower(user)) >>
          HTTP[F].post(gen[User].toUpper(user)) >>
          ExitCode.Success.pure[F]
      })
    } yield exitCode
}
