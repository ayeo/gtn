package pl.ayeo.gtn

import zio.console._
import zio.random._
import zio.{Runtime, ZIO}
import java.io.IOException

object Game extends App {
  def randomNumber(from: Int, to: Int): ZIO[Random, Nothing, Int] = nextIntBetween(from, to)

  case class GameState(userName: String, number: Int, guessNo: Int) {
    def fail(): GameState = new GameState(userName, number, guessNo + 1)
    def newNumber(number: Int): GameState = new GameState(userName, number, 0)

    def handle(guess: Int) =
      if (guess == number) manageSuccess(this) else manageFailure(this, guess)

    private def manageSuccess(gameState: GameState): ZIO[Console with Random, IOException, Unit] = {
      val message: String = gameState.guessNo match {
        case 0 => "Perfect!"
        case 1 => "Almost perfect"
        case 2 => "Nice"
        case n: Int => s"You have got it in ${n} rounds!"
      }

      putStrLn(message) *> wantToPlayMore
    }

    private def manageFailure(gameState: GameState, guess: Int): ZIO[Console with Random, IOException, Unit] = {
      val message = if (guess < gameState.number) "The number is higher" else "The number is lower"
      putStrLn(message) *> gameLoop(gameState.fail())
    }

    private def wantToPlayMore(): ZIO[Console with Random, IOException, Unit] = for {
        _ <- putStrLn("Want to play more?")
        _ <- getStrLn.flatMap(f => if (f == "y") ZIO.succeed(f) else ZIO.fail(new IOException("See you soon!")))
        number <- randomNumber(1, 4)
        _ <- gameLoop(newNumber(number))
      } yield ()
  }

  object GameState {
    def withName(name: String, number: Int) = new GameState(name, number, 0)
  }

  def getGuess(): ZIO[Console, IOException, Int] = {
    for {
      _ <- putStrLn("What is your guess?")
      n <- getStrLn.map(f => f.toInt)
      k <- if (n > 100 || n < 1) putStr("Number must be between 1-100") *> getGuess() else ZIO.succeed(n)
    } yield k
  }




  def gameLoop(gameState: GameState): ZIO[Console with Random, IOException, Unit] =
    for {
      guess <- getGuess
      x <- gameState.handle(guess)
    } yield x


  val init = for {
    //    _ <- putStrLn("Hello! What is your name?")
    //    name <- getStrLn
    number <- randomNumber(1, 4)
    _ <- gameLoop(GameState.withName("Albatros", number))
  } yield ()

  val runtime = Runtime.default
  runtime.unsafeRun(init.catchAll(t => putStrLn(t.getMessage)))
  //runtime.unsafeRun(init)
}