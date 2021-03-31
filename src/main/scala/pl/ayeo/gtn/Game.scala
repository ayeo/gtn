package pl.ayeo.gtn

import zio.console._
import zio.random._
import zio.{Runtime, ZIO}

import java.io.IOException
import scala.util.{Failure, Success, Try}

case class GameState(userName: String, number: Int, guessNo: Int) {
  def fail(): GameState = new GameState(userName, number, guessNo + 1)
  def newNumber(number: Int): GameState = new GameState(userName, number, 0)
}

object GameState {
  def withName(name: String, number: Int) = new GameState(name, number, 0)
}

object Game extends App {
  def randomNumber(from: Int, to: Int): ZIO[Random, Nothing, Int] = nextIntBetween(from, to)

  def handleA(gameState: GameState): ZIO[Console with Random, Throwable, Unit] = {
    val message: String = gameState.guessNo match {
      case 0 => "Perfect!"
      case 1 => "Almost perfect"
      case 2 => "Nice"
      case n: Int => s"You have got it in ${n} rounds!"
    }

    putStrLn(message) *> wantToPlayMore(gameState)
  }

  def handleB(gameState: GameState, guess: Int): ZIO[Console with Random, Throwable, Unit] = {
    val message = if (guess < gameState.number) "The number is higher" else "The number is lower"
    putStrLn(message) *> gameLoop(gameState.fail())
  }

  def wantToPlayMore(state: GameState): ZIO[Console with Random, Throwable, Unit] = for {
    _ <- putStrLn("Want to play more?")
    _ <- getStrLn.flatMap(f => if (f == "y") ZIO.succeed(f) else ZIO.fail(new IOException("See you soon!")))
    number <- randomNumber(1, 4)
    _ <- gameLoop(state.newNumber(number))
  } yield ()

  def getGuess(name: String): ZIO[Console, Throwable, Int] = {
    for {
      _ <- putStrLn(s"What is your guess, ${name}?")
      n <- getStrLn
      nn <- Try(n.toInt) match {
        case Success(answer) => ZIO.succeed(answer)
        case Failure(e) => putStrLn("You are supposed to enter a number") *> getGuess(name)
      }
      k <- if (nn > 100 || nn < 1) putStrLn("Number must be between 1-100") *> getGuess(name) else ZIO.succeed[Int](nn)
    } yield k
  }

  def gameLoop(state: GameState): ZIO[Console with Random, Throwable, Unit] =
    for {
      guess <- getGuess(state.userName)
      _ <- if (guess == state.number) handleA(state) else handleB(state, guess)
    } yield ()


  val init = for {
    _ <- putStrLn("Hello! What is your name?")
    name <- getStrLn
    number <- randomNumber(1, 100)
    _ <- gameLoop(GameState.withName(name, number))
  } yield ()

  val runtime = Runtime.default
  runtime.unsafeRun(init.catchAll(t => putStrLn(t.getMessage)))
}
