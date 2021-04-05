package pl.ayeo.gtn

import zio.console._
import zio.random._
import zio.Runtime
import zio.ZIO
import java.io.IOException
import scala.util.{Failure, Success, Try}

final case class GameState private(userName: Name, number: Number, guessNo: Int) {
  val fail: GameState = new GameState(userName, number, guessNo + 1)gi
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
  val getInt: Int = number
}
object Number {
  val from: Int = 1
  val to: Int = 100
  def make(number: Int): Either[String, Number] =
    if (number < from || number > to) Left(s"Number must be between $from-$to")
    else Right(Number(number))

  def fromString(n: String): Either[String, Number] = {
    Try[Int](n.toInt) match {
      case Success(integer) => Number.make(integer)
      case Failure(_) => Left("This is not a number.")
    }
  }
}

object Game extends App {
  implicit def convertNumberToInt(number: Number): Int = number.getInt

  def getName(message: String): ZIO[Console, IOException, Name] = for {
    _     <- putStrLn(message)
    input <- getStrLn
    name  <- ZIO.fromOption(Name.make(input)) <> getName("Invalid name, please try again")
  } yield name

  def guess(message: String): ZIO[Console, IOException, Number] = for {
    _       <- putStrLn(message)
    input   <- getStrLn
    number  <- Number.fromString(input) match {
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
    loop(getLostMessage(guess, gameState.number), gameState.fail)
  }

  def wantToPlayMore(state: GameState): ZIO[Console with Random, Throwable, Unit] = for {
    _       <- putStrLn("Want to play more?")
    _       <- getStrLn.flatMap(f =>
      if (f == "y") ZIO.succeed(f)
      else ZIO.die(new IOException(s"See you soon. ${state.userName}!")))
    number  <- draw
    _       <- loop("Give me your guess", state.newNumber(number))
  } yield ()

  val draw: ZIO[Random, IOException, Number] = for {
    number <- nextIntBetween(Number.from, Number.to)
    n <- ZIO.fromEither(Number.make(number)) <> draw
  } yield n

  def loop(message: String, state: GameState): ZIO[Console with Random, Throwable, Unit] = for {
    guess   <- guess(message)
    _       <- if (guess == state.number) victory(state) else lost(state, guess)
  } yield ()

  val init = for {
    name    <- getName("Hello! What is your name?")
    number  <- draw
    _       <- loop(s"What is your first guess, $name?", GameState.withName(name, number))
  } yield ()

  val runtime = Runtime.default
  runtime.unsafeRun(init.catchAll(t => putStrLn(t.getMessage)))
}
