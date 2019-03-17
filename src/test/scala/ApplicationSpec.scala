import Application.{Console, HTTP, User}
import cats.data._
import cats.effect.ExitCode
import cats.implicits._
import org.scalatest.FunSuite

class ApplicationSpec extends FunSuite {
  final case class TestData(httpInput: String,
                            httpOutput: Option[User] = None,
                            consoleOutput: Option[User] = None)

  type Effect[A] = State[TestData, A]

  implicit val testHttp: HTTP[Effect] = new HTTP[Effect] {
    override def get: Effect[String] =
      State(t => (t, t.httpInput))
    override def post(user: User): Effect[Unit] =
      State(t => (t.copy(httpOutput = user.some), ()))
  }

  implicit val testConsole: Console[Effect] = (user: User) =>
    State(t => (t.copy(consoleOutput = user.some), ()))

  val correctInput = TestData(httpInput = """
     |{
     | "id": "abc123",
     | "name": "Igor Ramazanov",
     | "address": {
     |   "city": "St. Petersburg",
     |   "zip": 123456,
     |   "street": "Lenina"
     | }
     |}
   """.stripMargin)

  val incorrectInput = TestData(httpInput = """
     |{
     | "id": "abc123",
     | "name": "Igor Ramazanov",
     | "address": {
     |   "city": "St. Petersburg",
     |   "zip": 123456
     | }
     |}
   """.stripMargin)

  test("""|Application.program should make HTTP POST request
          |with upper case and print to console in lowercase""".stripMargin) {
    val (result, exitCode) = Application.program[Effect].run(correctInput).value
    assert(exitCode == ExitCode.Success)
    assert(result.httpOutput.get.id == "ABC123")
    assert(result.httpOutput.get.name == "IGOR RAMAZANOV")
    assert(result.httpOutput.get.address.city == "ST. PETERSBURG")
    assert(result.httpOutput.get.address.zip == 123456)
    assert(result.httpOutput.get.address.street == "LENINA")

    assert(result.consoleOutput.get.id == "abc123")
    assert(result.consoleOutput.get.name == "igor ramazanov")
    assert(result.consoleOutput.get.address.city == "st. petersburg")
    assert(result.consoleOutput.get.address.zip == 123456)
    assert(result.consoleOutput.get.address.street == "lenina")
  }

  test("""|Application.program should handle JSON parsing errors
          |and return ExitCode.Error""".stripMargin) {
    val exitCode = Application.program[Effect].runA(incorrectInput).value
    assert(exitCode == ExitCode.Error)
  }

  test("""|toLower and toUpper should work for
       |any arbitrary case classes and sealed traits hierarchy""".stripMargin) {
    sealed trait Test
    final case class A(a: String, b: String) extends Test
    final case class B(c: String, d: String) extends Test
    import Application.GenericCaseConverter._

    assert(gen[Test].toLower(A("A", "B")) == A("a", "b"))
    assert(gen[Test].toUpper(A("a", "b")) == A("A", "B"))

    assert(gen[Test].toLower(B("C", "D")) == B("c", "d"))
    assert(gen[Test].toUpper(B("c", "d")) == B("C", "D"))
  }
}
