package pl.ayeo.gtn

import pl.ayeo.gtn.Game.{guess, lost, victory}
import zio.console._
import zio.random._
import zio.Runtime
import zio.ZIO

import java.io.IOException
import scala.util.{Failure, Success, Try}

final case class GameState private(userName: Name, number: Number, guessNo: Int) {
  def fail(): GameState = new GameState(userName, number, guessNo + 1)

  def newNumber(number: Number): GameState = new GameState(userName, number, 0)
}
object GameState {
  def withName(name: Name, number: Number) = new GameState(name, number, 0)
}

final case class Name private(name: String)
object Name {
  def make(name: String): Option[Name] = if (name.length < 2) None else Some(Name(name))
}

final case class Number private(number: Int) {
  def <(other: Number): Boolean = number < other.number
}
object Number {
  def make(number: Int): Either[String, Number] =
    if (number < 1 || number > 100) Left("Number must be between 1-100")
    else Right(Number(number))

  def fromString(n: String): Either[String, Number] = {
    Try[Int](n.toInt) match {
      case Success(integer) => Number.make(integer)
      case Failure(_) => Left("This is not a number.")
    }
  }
}

object Game extends App {
  def getName(message: String): ZIO[Console, IOException, Name] = for {
    _ <- putStrLn(message)
    input <- getStrLn
    name <- ZIO.fromOption(Name.make(input)) <> getName("Invalid name, please focus more...")
  } yield name

  def guess(message: String): ZIO[Console, IOException, Number] = for {
    _ <- putStrLn(message)
    input <- getStrLn
    number <- Number.fromString(input) match {
      case Right(n) => ZIO.succeed(n)
      case Left(message) => guess(message)
    }
  } yield number


  def getVictoryMessage(n: Int): String = n match {
    case 0 => "Perfect!"
    case 1 => "Almost perfect"
    case 2 => "Nice"
    case n: Int => s"You have got it in ${n} rounds!"
  }

  def getLostMessage(given: Number, expected: Number) =
    if (given < expected) "The number is higher" else "The number is lower"

  def victory(gameState: GameState): ZIO[Console with Random, Throwable, Unit] =
    putStrLn(getVictoryMessage(gameState.guessNo)) *> wantToPlayMore(gameState)

  def lost(gameState: GameState, guess: Number): ZIO[Console with Random, Throwable, Unit] = {
    loop(getLostMessage(guess, gameState.number), gameState.fail())
  }

  def wantToPlayMore(state: GameState): ZIO[Console with Random, Throwable, Unit] = for {
    _       <- putStrLn("Want to play more?")
    _       <- getStrLn.flatMap(f => if (f == "y") ZIO.succeed(f) else ZIO.fail(new IOException("See you soon!")))
    number  <- drawNewNumber()
    _       <- loop("Give me your guess", state.newNumber(number))
  } yield ()

  def drawNewNumber(): ZIO[Random, IOException, Number] = for {
    number <- nextIntBetween(1, 100)
    n <- Number.make(number) match {
      case Right(x) => ZIO.succeed(x)
      case Left(_) => drawNewNumber()
    }
  } yield n

  def loop(message: String, state: GameState) = for {
    guess   <- guess(message)
    _       <- if (guess == state.number) victory(state) else lost(state, guess)
  } yield ()

  val init = for {
    name    <- getName("Hello! What is your name?")
    number  <- drawNewNumber()
    _       <- loop("What is your first guess?", GameState.withName(name, number))
  } yield ()

  val runtime = Runtime.default
  runtime.unsafeRun(init.catchAll(t => putStrLn(t.getMessage)))
}
