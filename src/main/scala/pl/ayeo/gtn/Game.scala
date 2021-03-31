package pl.ayeo.gtn

import zio.console._
import zio.random._
import zio.{Runtime, ZIO}
import java.io.IOException

object Game extends App {
  def randomNumber(from: Int, to: Int): ZIO[Random, Nothing, Int] = nextIntBetween(from, to)

  case class GameState(userName: String, number: Int, guessNo: Int) {
    def fail(): GameState = new GameState(userName, number, guessNo + 1)
  }

  object GameState {
    def withName(name: String, number: Int) = new GameState(name, number, 0)
  }

  def getGuess(): ZIO[Console, IOException, Int] = {
    for {
      _ <- putStrLn("Gimme your bet bitch..")
      n <- getStrLn.map(f => f.toInt)
      k <- if (n > 100) putStr("Stupid or wat?") *> getGuess() else ZIO.succeed(n)
    } yield k
  }

  def manageSuccess(gameState: GameState): ZIO[Console with Random, IOException, Unit] = gameState.guessNo match {
    case 0 => putStrLn(s"You lucky bustard!")
    case 1 => putStrLn(s"Not bad")
    case 2 => putStrLn(s"So, so")
    case n: Int  => putStrLn(s"You lame! It took ${n} rounds!")
  }

  def manageFailure(gameState: GameState, guess: Int): ZIO[Console with Random, IOException, Unit] = {
    val message = if (guess < gameState.number) "The number is higher" else "Bit to much"
    putStrLn(message) *> gameLoop(gameState.fail())
  }


  def gameLoop(gameState: GameState): ZIO[Console with Random, IOException, Unit] = {
    getGuess.flatMap(guess => {
      if (guess == gameState.number) manageSuccess(gameState)
      else manageFailure(gameState, guess)
    })
  }

  val init = for {
//    _ <- putStrLn("Hello! What is your name?")
//    name <- getStrLn
    number <- randomNumber(1, 100)
    _ <- gameLoop(GameState.withName("Albatros", number))
  } yield ()

  val runtime = Runtime.default

  runtime.unsafeRun(init)
}