package com.example

import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite
import scala.concurrent.duration._

class HelloWorldSuite extends CatsEffectSuite {

  test("1 + 1 == 2") {

    import cats.effect.std.IOAsync._

    val hello = IO.sleep(2.second) >> IO(1)

    val foobar = async(await(hello) + await(hello))

    foobar.map(it => assertEquals(it, 2))
  }
}
