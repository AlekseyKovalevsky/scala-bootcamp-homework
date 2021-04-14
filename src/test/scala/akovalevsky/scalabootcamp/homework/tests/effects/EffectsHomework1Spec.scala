package akovalevsky.scalabootcamp.homework.tests.effects

import akovalevsky.scalabootcamp.homework.effects.EffectsHomework1.IO
import cats.implicits.toFunctorOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

class EffectsHomework1Spec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  def createCallLogger[A](callResultLog: mutable.Queue[(A, String)]): (=> A, String) => A = (f, fId) => {
    val res = f
    callResultLog.enqueue((res, fId))
    res
  }

  "IO.map" should "work correctly" in {
    forAll { (x: Int, f: Int => Int) =>
      IO(x).map(f).unsafeRunSync() shouldEqual f(x)
    }
  }

  "IO.flatMap" should "compose actions into a sequence" in {
    forAll { (x: Int, y: Int => Int, z: Int => Int) =>
      val res = for {
        x <- IO.pure(x)
        yRes <- IO.pure(y(x))
        zRes <- IO.pure(z(yRes))
      } yield zRes

      res.unsafeRunSync() shouldEqual y.andThen(z)(x)
    }
  }

  "IO.*>" should "run the current IO then run the parameter" in {
    forAll { (x: Int, y: Int) =>
      val callResultLog = mutable.Queue[(Int, String)]()
      val callAndLog = createCallLogger(callResultLog)

      val io = IO(callAndLog(x, "first")) *> IO(callAndLog(y, "second"))

      io.unsafeRunSync() shouldEqual y
      callResultLog.toList shouldEqual List((x, "first"), (y, "second"))
    }
  }

  "IO.as" should "run the current IO and replace result by parameter" in {
    forAll { (x: Int, y: Int) =>
      val callResultLog = mutable.Queue[(Int, String)]()
      val callAndLog = createCallLogger(callResultLog)

      val io = IO(callAndLog(x, "x")).as(y)

      io.unsafeRunSync() shouldEqual y
      callResultLog.toList shouldEqual List((x, "x"))
    }
  }

  "IO.void" should "run the current IO and ignore it's result" in {
    forAll { (x: Int) =>
      val callResultLog = mutable.Queue[(Int, String)]()
      val callAndLog = createCallLogger(callResultLog)

      val io = IO(callAndLog(x, "x")).void

      io.unsafeRunSync() shouldEqual()
      callResultLog.toList shouldEqual List((x, "x"))
    }
  }

  "IO.attempt" should "catch the exception occurred during the current IO run and return Left(Throwable)" in {
    val ex = new Exception()
    val io = IO(throw ex).attempt

    io.unsafeRunSync() shouldEqual Left(ex)
  }

  "IO.attempt" should "return the current IO runz wrapped in Right()" in {
    forAll { (x: Int) =>
      val io = IO(x).attempt

      io.unsafeRunSync() shouldEqual Right(x)
    }
  }

  "IO.option" should "catch the exception occurred during the current IO run and return None" in {
    val io = IO(throw new Exception()).option

    io.unsafeRunSync() shouldEqual None
  }

  "IO.option" should "return the current IO run wrapped in Some()" in {
    forAll { (x: Int) =>
      val io = IO(x).option

      io.unsafeRunSync() shouldEqual Some(x)
    }
  }

  "IO.handleErrorWith" should "catch the exception and run the IO specified as the parameter" in {
    forAll { (x: Int) =>
      val io = IO(throw new Exception()).handleErrorWith(_ => IO(x))

      io.unsafeRunSync() shouldEqual x
    }
  }

  "IO.redeem" should "catch the exception and transform it to the value with the recover function parameter" in {
    forAll { (x: Int) =>
      val io = IO[Int](throw new Exception()).redeem(_ => x, identity)

      io.unsafeRunSync() shouldEqual x
    }
  }

  "IO.redeem" should "transform the result with the map function parameter" in {
    forAll { (x: Int, y: Int => Int) =>
      val io = IO(x).redeem(_ => 0, y)

      io.unsafeRunSync() shouldEqual y(x)
    }
  }

  "IO.redeemWith" should "catch the exception and run the recover IO parameter" in {
    forAll { (recoverVal: Int, bindVal: Int) =>
      val io = IO[Int](throw new Exception()).redeemWith(_ => IO(recoverVal), _ => IO(bindVal))

      io.unsafeRunSync() shouldEqual recoverVal
    }
  }

  "IO.redeemWith" should "maps the current IO result to the new IO via bind and runs the new IO" in {
    forAll { (x: Int, recoverVal: Int, bind: Int => Int) =>
      val io = IO[Int](x).redeemWith(_ => IO(recoverVal), bind.map(IO(_)))

      io.unsafeRunSync() shouldEqual bind(x)
    }
  }

  "IO.unsafeRunSync" should "execute the underlying run function" in {
    forAll { (f: () => Int) =>
      val io = IO(f())

      io.unsafeRunSync() shouldEqual f()
    }
  }

  "IO.unsafeToFuture" should "return the future executing the underlying run function" in {
    import scala.concurrent.ExecutionContext.Implicits.global

    forAll { (f: () => Int) =>
      val future = IO(f()).unsafeToFuture

      Await.result(future, 10.seconds) shouldEqual f()
    }
  }

  "IO.apply/delay" should "build the IO executing the run function parameter" in {
    forAll { (x: Int, y: Int => Int) =>

      IO(y(x)).unsafeRunSync() shouldEqual y(x)
      IO.delay(y(x)).unsafeRunSync() shouldEqual y(x)
    }
  }

  "IO.suspend" should "build the IO executing the thunk function parameter then running the result of the thunk" in {
    forAll { (x: Int, y: Int => Int) =>
      val io = IO.suspend(IO(y(x)))

      io.unsafeRunSync() shouldEqual y(x)
    }
  }

  "IO.pure" should "wrap a value into the IO by executing () => A" in {
    forAll { (x: Int) =>
      val io = IO.pure(x)

      io.unsafeRunSync() shouldEqual x
    }
  }

  "IO.fromEither" should "build IO throwing exception if Left(ex) and returning x if Right(x) passed to it" in {
    forAll { (x: Int) =>
      assertThrows[Exception](IO.fromEither[Unit](Left(new Exception())).unsafeRunSync())
      IO.fromEither(Right(x)).unsafeRunSync() shouldEqual x
    }
  }

  "IO.fromOption" should "build IO throwing exception if None and returning x if Some(x) passed to it" in {
    forAll { (x: Int) =>
      assertThrows[Exception](IO.fromOption(None)(new Exception()).unsafeRunSync())
      IO.fromOption(Some(x))(new Exception()).unsafeRunSync() shouldEqual x
    }
  }

  "IO.fromTry" should "build IO throwing exception if Failure(ex) and returning x if Success(x) passed to it" in {
    forAll { (x: Int) =>
      assertThrows[Exception](IO.fromTry(Failure(new Exception())).unsafeRunSync())
      IO.fromTry(Success(x)).unsafeRunSync() shouldEqual x
    }
  }

  "IO.none" should "build IO returning None[A]" in {
    val io = IO.none

    io.unsafeRunSync() shouldEqual None
  }

  "IO.raiseError" should "build IO throwing error" in {
    val io = IO.raiseError(new Exception())

    assertThrows[Exception](io.unsafeRunSync())
  }

  "IO.raiseUnless" should "throw exception if cond is false" in {
    val io = IO.raiseUnless(cond = false)(new Exception())

    assertThrows[Exception](io.unsafeRunSync())
  }

  "IO.raiseWhen" should "throw exception if cond is true" in {
    val io = IO.raiseWhen(cond = true)(new Exception())

    assertThrows[Exception](io.unsafeRunSync())
  }

  "IO.unlessA" should "run action if cond is false" in {
    val callResultLog = mutable.Queue[(Unit, String)]()
    val callAndLog = createCallLogger(callResultLog)

    IO.unlessA(cond = false)(IO(callAndLog({}, "run"))).unsafeRunSync()

    callResultLog.toList shouldEqual List(((), "run"))
  }
}

