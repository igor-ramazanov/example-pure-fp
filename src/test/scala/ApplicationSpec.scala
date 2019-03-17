import Application.{Address, Console, HTTP, User}
import cats.data._
import cats.effect.ExitCode
import cats.implicits._
import org.scalatest.FunSuite

class ApplicationSpec extends FunSuite {
  final case class TestData(getInput: String,
                            postOutput: Option[User] = None,
                            consoleOutput: Option[User] = None)

  type Effect[A] = State[TestData, A]

  def state[A](f: TestData => (TestData, A)): State[TestData, A] =
    State[TestData, A](f)

  implicit val testHttp: HTTP[Effect] = new HTTP[Effect] {
    override def get: Effect[String] =
      state(t => (t, t.getInput))
    override def post(user: User): Effect[Unit] =
      state(t => (t.copy(postOutput = user.some), ()))
  }

  implicit val testConsole: Console[Effect] = (user: User) =>
    state(t => (t.copy(consoleOutput = user.some), ()))

  test(
    "Application.program should make HTTP POST request with upper case and print to console in lowercase"
  ) {
    val testData = TestData(getInput = """
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
    val (result, exitCode) = Application.program[Effect].run(testData).value
    assert(exitCode == ExitCode.Success)
    assert(
      result.postOutput.contains(
        User(
          "ABC123",
          "IGOR RAMAZANOV",
          Address("ST. PETERSBURG", 123456, "LENINA")
        )
      )
    )
    assert(
      result.consoleOutput.contains(
        User(
          "abc123",
          "igor ramazanov",
          Address("st. petersburg", 123456, "lenina")
        )
      )
    )
  }

  test(
    "Application.program handle JSON parsing errors and return ExitCode.Error") {
    val testData = TestData(getInput = """
                                         |{
                                         | "id": "abc123",
                                         | "name": "Igor Ramazanov",
                                         | "address": {
                                         |   "city": "St. Petersburg",
                                         |   "zip": 123456
                                         | }
                                         |}
                                       """.stripMargin)
    val (_, exitCode) = Application.program[Effect].run(testData).value
    assert(exitCode == ExitCode.Error)
  }
}
