package com.example

import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite
import cats.effect.std.IOAsync._
import scala.concurrent.duration._
import cats.syntax.all._
import cats.effect.kernel.Outcome

class HelloWorldSuite extends CatsEffectSuite {

  test("Success") {

    val hello = IO.sleep(1.second) >> IO(1)

    val foobar = async(await(hello) + await(hello))

    foobar.map(it => assertEquals(it, 2))
  }

  test("Error propagates") {

    case object Boom extends Throwable

    val foo = IO.sleep(1.second)
    val error = IO.defer(IO.raiseError[Int](Boom))

    val foobar = async { await(foo); await(error) }.recover { case Boom => 23 }

    foobar.map(it => assertEquals(it, 23))
  }

  test("Cancellation propagates outward") {

    val foobar = async { await(IO.canceled) }
    val expected = Outcome.canceled[IO, Throwable, Unit]

    foobar.start.flatMap(_.join).map(outcome => assertEquals(outcome, expected))
  }
}
