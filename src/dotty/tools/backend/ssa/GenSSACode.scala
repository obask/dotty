package dotty.tools.backend.ssa

import dotty.tools.dotc.{CompilationUnit, printing}
import dotty.tools.dotc.ast.tpd.Ident
import dotty.tools.dotc.ast.tpd.TypeDef
import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core._
import dotty.tools.dotc.printing.{PlainPrinter, RefinedPrinter}
import org.scalajs.core.ir.Trees.EmptyTree

import scala.collection.{Iterable, mutable}
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
//    val tree = ctx.compilationUnit.untpdTree


    //    val printer = new Printer()

    val printer = new RefinedPrinter(ctx)


    println("----------------")

    println(printer.toText(tree).mkString(100))

    println("----------------")

    def toSource(p: Any): String = {
      p match {
        case s: String => "\"" + s + "\""
        case () => "Unit"
        case Nil => "Nil"
        case ll: List[_] => ll.map(toSource).mkString("[# ", " ", "]")
        case tpd.EmptyTree => "EmptyTree"
        case t: tpd.TypeTree =>
          val tmp = if (t.hasType) toSource(t.typeOpt) else toSource(t.original)
          "(TypeTree " + tmp + ")"
        // else
        case p: Product => p.productIterator.map(toSource).mkString("(" + p.productPrefix + " ", " ", ")")
        case _ => p.toString
      }
    }

    println(toSource(tree))

    println("----------------")

    //    println("AST:")
//    tree match {
//      case xx: PackageDef =>
//        printer.printListField("AST", xx.stats)
//    }

    println(tree)

    println("----------------")
//    Lispyfy.processPackageDef(tree.asInstanceOf[PackageDef])


    entryPoints.clear()
  }




}
