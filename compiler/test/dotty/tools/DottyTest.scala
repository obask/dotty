package dotty
package tools

import dotc.core._
import dotc.core.Contexts._
import dotc.core.Symbols._
import dotc.core.Flags._
import Types._, Symbols._, Decorators._
import dotc.printing.Texts._
import dotc.reporting.ConsoleReporter
import dotc.core.Decorators._
import dotc.ast.tpd
import dotc.Compiler

import dotc.core.Phases.Phase

class DottyTest extends ContextEscapeDetection{

  dotc.parsing.Scanners // initialize keywords

  implicit var ctx: Contexts.Context = {
    val base = new ContextBase {}
    import base.settings._
    val ctx = base.initialCtx.fresh
    ctx.setSetting(ctx.settings.encoding, "UTF8")
    ctx.setSetting(ctx.settings.classpath, Jars.dottyLib)
    // when classpath is changed in ctx, we need to re-initialize to get the
    // correct classpath from PathResolver
    base.initialize()(ctx)
    ctx
  }

  override def getCtx: Context = ctx
  override def clearCtx() = {
    ctx = null
  }

  private def compilerWithChecker(phase: String)(assertion:(tpd.Tree, Context) => Unit) = new Compiler {
    override def phases = {
      val allPhases = super.phases
      val targetPhase = allPhases.flatten.find(p => p.phaseName == phase).get
      val groupsBefore = allPhases.takeWhile(x => !x.contains(targetPhase))
      val lastGroup = allPhases.find(x => x.contains(targetPhase)).get.takeWhile(x => !(x eq targetPhase))
      val checker = new Phase {
        def phaseName = "assertionChecker"
        override def run(implicit ctx: Context): Unit = assertion(ctx.compilationUnit.tpdTree, ctx)
      }
      val lastGroupAppended = List(lastGroup ::: targetPhase :: Nil)

      groupsBefore ::: lastGroupAppended ::: List(List(checker))
    }
  }

  def checkCompile(checkAfterPhase: String, source: String)(assertion: (tpd.Tree, Context) => Unit): Unit = {
    val c = compilerWithChecker(checkAfterPhase)(assertion)
    c.rootContext(ctx)
    val run = c.newRun
    run.compile(source)
  }

  def checkCompile(checkAfterPhase: String, sources:List[String])(assertion:(tpd.Tree, Context) => Unit): Unit = {
    val c = compilerWithChecker(checkAfterPhase)(assertion)
    c.rootContext(ctx)
    val run = c.newRun
    run.compile(sources)
  }

  def methType(names: String*)(paramTypes: Type*)(resultType: Type = defn.UnitType) =
    MethodType(names.toList map (_.toTermName), paramTypes.toList, resultType)
}
