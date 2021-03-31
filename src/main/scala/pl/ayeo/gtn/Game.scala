package pl.ayeo.gtn

import zio.console._
import zio.random._
import zio.Runtime
import zio.ZIO

import java.io.IOException
import scala.util.{Failure, Success, Try}

final case class GameState private (userName: String, number: Int, guessNo: Int) {
  def fail(): GameState = new GameState(userName, number, guessNo + 1)
  def newNumber(number: Int): GameState = new GameState(userName, number, 0)
}
object GameState {
  def withName(name: String, number: Int = 0) = new GameState(name, number, 0)
}

object Game extends App {
  def getVictoryMessage(n: Int): String = n match {
      case 0 => "Perfect!"
      case 1 => "Almost perfect"
      case 2 => "Nice"
      case n: Int => s"You have got it in ${n} rounds!"
  }

  def getLostMessage(given: Int, expected: Int) =
    if (given < expected) "The number is higher" else "The number is lower"

  def victory(gameState: GameState): ZIO[Console with Random, Throwable, Unit] =
    putStrLn(getVictoryMessage(gameState.guessNo)) *> wantToPlayMore(gameState)

  def lost(gameState: GameState, guess: Int): ZIO[Console with Random, Throwable, Unit] = {
    putStrLn(getLostMessage(guess, gameState.number)) *> gameLoop(gameState.fail())
  }

  def getGuess(name: String): ZIO[Console, Throwable, Int] = {
    for {
      _ <- putStrLn(s"What is your guess, ${name}?")
      rawInput <- getStrLn
      anNumber <- Try(rawInput.toInt) match {
        case Success(answer) => ZIO.succeed(answer)
        case Failure(e) => putStrLn("You are supposed to enter a number") *> getGuess(name)
      }
      validNumber <-
        if (anNumber > 100 || anNumber < 1) putStrLn("Number must be between 1-100") *> getGuess(name)
        else ZIO.succeed[Int](anNumber)
    } yield validNumber
  }

  def wantToPlayMore(state: GameState): ZIO[Console with Random, Throwable, Unit] = for {
    _ <- putStrLn("Want to play more?")
    _ <- getStrLn.flatMap(f => if (f == "y") ZIO.succeed(f) else ZIO.fail(new IOException("See you soon!")))
    _ <- drawNewNumber(state)
  } yield ()

  def drawNewNumber(state: GameState) = for {
    number <- nextIntBetween(1, 100)
    _ <- gameLoop(state.newNumber(number))
  } yield ()

  def gameLoop(state: GameState): ZIO[Console with Random, Throwable, Unit] =
    for {
      guess <- getGuess(state.userName)
      _ <- if (guess == state.number) victory(state) else lost(state, guess)
    } yield ()


  val init = for {
    _     <- putStrLn("Hello! What is your name?")
    name  <- getStrLn
    _     <- drawNewNumber(GameState.withName(name, 0))
  } yield ()

  val runtime = Runtime.default
  runtime.unsafeRun(init.catchAll(t => putStrLn(t.getMessage)))
}
