package cats.effect.std

import java.util.Objects

import scala.util.{Failure, Success, Try}
import cats.effect.std.Dispatcher
import cats.effect.IO
import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.Canceled
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Succeeded
import java.util.concurrent.CancellationException

import language.experimental.macros
import scala.annotation.compileTimeOnly
import scala.reflect.macros.blackbox
import scala.annotation.compileTimeOnly
import scala.reflect.macros.whitebox
import cats.syntax

object IOAsync {

  type Callback = Either[Throwable, AnyRef] => Unit

  /** Run the block of code `body` asynchronously. `body` may contain calls to `await` when the results of
    * a `Future` are needed; this is translated into non-blocking code.
    */
  def async[T](body: => T): IO[T] = macro asyncImpl[T]

  /** Non-blocking await the on result of `awaitable`. This may only be used directly within an enclosing `async` block.
    *
    * Internally, this will register the remainder of the code in enclosing `async` block as a callback
    * in the `onComplete` handler of `awaitable`, and will *not* block a thread.
    */
  @compileTimeOnly("[async] `await` must be enclosed in an `async` block")
  def await[T](awaitable: IO[T]): T =
    ??? // No implementation here, as calls to this are translated to `onComplete` by the macro.

  def asyncImpl[T: c.WeakTypeTag](c: whitebox.Context)(body: c.Tree): c.Tree = {
    import c.universe._
    if (!c.compilerSettings.contains("-Xasync")) {
      c.abort(
        c.macroApplication.pos,
        "The async requires the compiler option -Xasync (supported only by Scala 2.12.12+ / 2.13.3+)"
      )
    } else
      try {
        val awaitSym = typeOf[IOAsync.type].decl(TermName("await"))
        def mark(t: DefDef): Tree = {
          import language.reflectiveCalls
          c.internal
            .asInstanceOf[{
                def markForAsyncTransform(
                    owner: Symbol,
                    method: DefDef,
                    awaitSymbol: Symbol,
                    config: Map[String, AnyRef]
                ): DefDef
              }
            ]
            .markForAsyncTransform(
              c.internal.enclosingOwner,
              t,
              awaitSym,
              Map.empty
            )
        }
        val name = TypeName("stateMachine$async")
        // format: off
        q"""
          final class $name(dispatcher: _root_.cats.effect.std.Dispatcher[IO], callback: _root_.cats.effect.std.IOAsync.Callback) extends _root_.cats.effect.std.IOStateMachine(dispatcher, callback) {
            ${mark(q"""override def apply(tr$$async: _root_.scala.Either[_root_.scala.Throwable, _root_.scala.AnyRef]): _root_.scala.Unit = ${body}""")}
          }
          _root_.cats.effect.std.Dispatcher[IO].use { dispatcher =>
            _root_.cats.effect.IO.async_[_root_.scala.AnyRef](cb => new $name(dispatcher, cb).start())
          }.asInstanceOf[${c.macroApplication.tpe}]
        """
      } catch {
        case e: ReflectiveOperationException =>
          c.abort(
            c.macroApplication.pos,
            "-Xasync is provided as a Scala compiler option, but the async macro is unable to call c.internal.markForAsyncTransform. " + e.getClass.getName + " " + e.getMessage
          )
      }
  }
}

abstract class IOStateMachine(
    dispatcher: Dispatcher[IO],
    callback: IOAsync.Callback
) extends Function1[Either[Throwable, AnyRef], Unit] {

  // FSM translated method
  //def apply(v1: Either[Throwable, AnyRef]): Unit = ???

  private[this] var state$async: Int = 0

  /** Retrieve the current value of the state variable */
  protected def state: Int = state$async

  /** Assign `i` to the state variable */
  protected def state_=(s: Int): Unit = state$async = s

  protected def completeFailure(t: Throwable): Unit =
    callback(Left(t))

  protected def completeSuccess(value: AnyRef): Unit =
    callback(Right(value))

  protected def onComplete(f: IO[AnyRef]): Unit = {
    dispatcher.unsafeRunAndForget(f.attempt.flatMap(either => IO(this(either))))
  }

  protected def getCompleted(f: IO[AnyRef]): Either[Throwable, AnyRef] = {
    null
  }

  protected def tryGet(tr: Either[Throwable, AnyRef]): AnyRef =
    tr match {
      case Right(value) =>
        value.asInstanceOf[AnyRef]
      case Left(throwable) =>
        callback(Left(throwable))
        this // sentinel value to indicate the dispatch loop should exit.
    }

  def start(): Unit = {
    // `def apply` does not consult its argument when `state == 0`.
    apply(null)
  }

}
