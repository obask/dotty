package dotty.tools
package dottydoc
package core

import dotc.core.Symbols.Symbol
import dotc.core.Comments.ContextDocstrings
import model.Package

class ContextDottydoc extends ContextDocstrings {
  import scala.collection.mutable

  private[this] val _packages: mutable.Map[String, Package] = mutable.Map.empty
  def packages: Map[String, Package] = _packages.toMap
  def packagesMutable: mutable.Map[String, Package] = _packages

  /** Should perhaps factorize this into caches that get flushed */
  private var _defs: Map[Symbol, Set[Symbol]] = Map.empty
  def defs(sym: Symbol): Set[Symbol] = _defs.get(sym).getOrElse(Set.empty)

  def addDef(s: Symbol, d: Symbol): Unit = _defs = (_defs + {
    s -> _defs.get(s).map(xs => xs + d).getOrElse(Set(d))
  })
}
