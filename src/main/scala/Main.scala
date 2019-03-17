import Application.{Console, HTTP, User}
import cats.effect._
object Main extends IOApp {

  /*Concrete implementation (simplified for brevity) of HTTP effect*/
  implicit val ioHttp: HTTP[IO] = new HTTP[IO] {
    override def get: IO[String] = IO {
      """
        |{
        | "id": "abc123",
        | "name": "Igor Ramazanov",
        | "address": {
        |   "city": "St. Petersburg",
        |   "zip": 123456,
        |   "street": "Lenina"
        | }
        |}
      """.stripMargin
    }
    override def post(user: User): IO[Unit] = IO {
      println("POST: " + user)
    }
  }

  /*Concrete implementation of console printing effect */
  implicit val ioConsole: Console[IO] = (user: User) =>
    IO(println("CONSOLE: " + user))

  /*Running program substituting concrete effect type*/
  override def run(args: List[String]): IO[ExitCode] = Application.program[IO]
}
