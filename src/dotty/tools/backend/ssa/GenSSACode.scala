package dotty.tools.backend.ssa

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.tpd.Ident
import dotty.tools.dotc.ast.tpd.TypeDef
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core._

import scala.collection.mutable
import scala.tools.asm
import scala.tools.asm.CustomAttr
import scala.tools.asm.tree._
import scala.tools.nsc.Settings
import scala.tools.nsc.backend.jvm._
import scala.tools.nsc.backend.jvm.opt.LocalOpt

class GenSSACode extends Phase {
  def phaseName: String = "genSSACode"
  private val entryPoints = new mutable.HashSet[Symbol]()
  def registerEntryPoint(sym: Symbol) = entryPoints += sym


  def run(implicit ctx: Context): Unit = {
//    new GenBCodePipeline(entryPoints.toList, new DottyBackendInterface()(ctx))(ctx).run(ctx.compilationUnit.tpdTree)


    val tree = ctx.compilationUnit.tpdTree

    val printer = new Printer()

    println(tree.show)

//    println("AST:")
//    tree match {
//      case xx: PackageDef =>
//        printer.printListField("AST", xx.stats)
//    }

    println("----------------")
    Lispyfy.processPackageDef(tree.asInstanceOf[PackageDef])


    entryPoints.clear()
  }




}
