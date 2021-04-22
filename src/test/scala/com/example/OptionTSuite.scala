package com.example

import cats.effect.IO
import munit.CatsEffectSuite
import cats.effect.std.AsyncAwaitDsl
import cats.data.OptionT

class OptionTSuite extends CatsEffectSuite {

  type Foo[A] = OptionT[IO, A]
  object dsl extends AsyncAwaitDsl[Foo]
  import dsl._

  test("optionT") {

    val io: Foo[Int] = OptionT.none[IO, Int]

    val foo = async(await(io))

    foo.value.map(it => assertEquals(it, None))
  }
}
