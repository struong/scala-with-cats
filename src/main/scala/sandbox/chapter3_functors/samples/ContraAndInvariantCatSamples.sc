import cats.Contravariant
import cats.Show
import cats.instances.string._

val showString = Show[String]
val showSymbol = Contravariant[Show]
  .contramap(showString)((sym: Symbol) => s"'${sym.name}'")

showSymbol.show(Symbol("Dave"))

import cats.syntax.contravariant._ // for contramap

showString
  .contramap[Symbol](sym => s"'${sym.name}'")
  .show(Symbol("dave"))

import cats.Monoid
import cats.instances.all._ // for Monoid
import cats.syntax.invariant._ // for imap
import cats.syntax.semigroup._ // for |+|

implicit val symbolMonoid: Monoid[Symbol] =
  Monoid[String].imap(Symbol.apply)(_.name)

Monoid[Symbol].empty
Symbol("a") |+| Symbol(" few ") |+| Symbol("words")
