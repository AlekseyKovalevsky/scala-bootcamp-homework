package akovalevsky.scalabootcamp.homework.effects

import cats.data.ValidatedNec
import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxValidatedIdBinCompat0
import cats.implicits.catsSyntaxParallelTraverse

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.io.{BufferedSource, Source, StdIn}
import scala.reflect.io.File

object EffectsHomework2 extends IOApp {

  object Validation {
    type ValidationError = String
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validateSeedIsNumber(seed: String): AllErrorsOr[Int] =
      seed.toIntOption.fold("The seed is not a number".invalidNec[Int])(_.validNec)

    def validateSeedIsNotNegative(seed: Int): AllErrorsOr[Int] =
      if (seed > 0) seed.validNec
      else "The seed is not a positive number".invalidNec

    def validateFilePathExists(filePath: String): AllErrorsOr[String] =
      if (File(filePath).exists) filePath.validNec
      else "The file path doesn't exist".invalidNec
  }

  object Hash {
    def knuthHash(word: String, constant: Int): Int = {
      // for debugging purposes
      println(s"knuth hash on thread ${Thread.currentThread.getName}")

      var hash = 0
      for (ch <- word.toCharArray)
        hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
      hash % constant
    }

    def javaHash(word: String, seed: Int = 0): Int = {
      // for debugging purposes
      println(s"java hash on thread ${Thread.currentThread.getName}")

      var hash = 0
      for (ch <- word.toCharArray)
        hash = 31 * hash + ch.toInt
      hash = hash ^ (hash >> 20) ^ (hash >> 12)
      hash ^ (hash >> 7) ^ (hash >> 4)
    }

    val hashFuncs: List[(String, Int) => Int] = List(knuthHash, javaHash)
  }

  import Validation._
  import Hash._

  def readFilePathUnlessValid(blocker: Blocker): IO[String] = blocker.blockOn {
    for {
      _ <- printThreadName
      pathStr <- IO(println("Enter the file path:")) *> IO(StdIn.readLine())
      pathStrValidated <- IO(validateFilePathExists(pathStr))
      validPathStr <- pathStrValidated.fold(errs => {
        IO(println("Invalid file path:")) *>
          IO(println(errs.show)) *>
          readFilePathUnlessValid(blocker)
      }, IO(_))
    } yield validPathStr
  }


  def readSeedUnlessValid(blocker: Blocker): IO[Int] = blocker.blockOn {
    for {
      _ <- printThreadName
      seedStr <- IO(println("Enter the seed:")) *> IO(StdIn.readLine())
      seedStrValidated <- IO(validateSeedIsNumber(seedStr).andThen(validateSeedIsNotNegative))
      validSeedStr <- seedStrValidated.fold(errs => {
        IO(println("Invalid seed string:")) *>
          IO(println(errs.show)) *>
          readSeedUnlessValid(blocker)
      }, IO(_))
    } yield validSeedStr
  }

  def printThreadName: IO[Unit] = IO(println(s"on thread ${Thread.currentThread.getName}"))

  def calculateMinHashes(src: BufferedSource, seed: Int)(implicit contextShift: ContextShift[IO]): IO[List[Int]] =
    for {
      words <- IO {
        src.getLines().toList
          .flatMap(line => line.split("\n+"))
          .filter(_.length > 2)
      }
      _ <- IO.raiseWhen(words.isEmpty)(new Exception("The file is empty! Can't calculate min hashes."))
      wordsHashes <- words.parTraverse(word => {
        hashFuncs.parTraverse(func => IO.shift *> IO(func(word, seed)))
      })
    } yield wordsHashes.foldLeft(List.fill(hashFuncs.length)(Int.MaxValue)) {
      (minHashes, currentHashes) => {
        minHashes.zip(currentHashes).map { hashes =>
          val (minHash, currentHash) = hashes
          minHash min currentHash
        }
      }
    }


  override def run(args: List[String]): IO[ExitCode] = {

    def blockingEC =
      ExecutionContext.fromExecutorService(
        Executors.newCachedThreadPool((r: Runnable) => {
          val t = new Thread(r)
          t.setName(s"blocking-io-ec-${
            t.getName
          }")
          t
        }))

    def cpuBoundEC =
      ExecutionContext.fromExecutorService(
        Executors.newFixedThreadPool(8, (r: Runnable) => {
          val t = new Thread(r)
          t.setName(s"cpu-bound-ec-${
            t.getName
          }")
          t
        }))

    // tried to use both blocker and context shift APIs
    (for {
      filePath <- Blocker.fromExecutorService[IO](IO(blockingEC)).use(readFilePathUnlessValid)
      _ <- printThreadName
      seed <- Blocker.fromExecutorService[IO](IO(blockingEC)).use(readSeedUnlessValid)
      minHashes <- Resource.fromAutoCloseable(IO(Source.fromFile(filePath))).use {
        source =>
          calculateMinHashes(source, seed)(IO.contextShift(cpuBoundEC))
            .guarantee(IO.shift) // shift back anyway
      }
      _ <- printThreadName
      _ <- IO(println(minHashes))
    } yield ()) as ExitCode.Success
  }
}
