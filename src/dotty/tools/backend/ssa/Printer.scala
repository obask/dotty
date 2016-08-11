package dotty.tools.backend.ssa

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts.Context

class Printer(implicit ctx: Context) {


  def dumpTree(tree: Tree): Unit = {
    printHeader(tree)
    tree match {
      case xx: Template =>
        dumpTemplate(xx)
      case xx: TypeDef =>
        dumpTypeDef(xx)
      case xx: ValDef =>
        dumpValDef(xx)
      case xx: Apply =>
        dumpApply(xx)
      case xx: TypeTree =>
        printString("value", xx.toString)
      case xx: DefDef =>
        dumpDefDef(xx)
      case xx: Block =>
        dumpBlock(xx)
      case xx =>
        printString("value", xx.toString)
    }
    print("}")
  }

  def printHeader(tt: Tree) = {
    val header = tt.getClass.toString.replace("$", "@#%").split("@#%").last
    println("{\"$type\": \"" + header + "\"")
  }

  def printField(name: String, value: Tree) = {
    print(",\"" + name + "\":")
    dumpTree(value)
  }

  def printString(name: String, value: String) = {
    print(",\"" + name + "\": \"" + value.replace("\"", "_") + "\"")
  }

  def printListField(name: String, tt: List[Tree]) = {
    print(",\"" + name + "\": [")
    if (tt.nonEmpty) {
      dumpTree(tt.head)
      tt.tail.foreach { xx =>
        print(", ")
        dumpTree(xx)
      }
    }
    print("]")
  }

  // case class Block[-T >: Untyped] private[ast] (stats: List[Tree[T]], expr: Tree[T])
  def dumpBlock(tree: Block) = {
    printListField("stats", tree.stats)
    printField("expr", tree.expr)
  }


  // DefDef[-T >: Untyped] private[ast] (name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], private var preRhs: LazyTree)
  def dumpDefDef(tree: DefDef): Unit = {
    printString("name", tree.name.toString)
    printListField("tparams", tree.tparams)
    printString("vparamss", tree.vparamss.toString())
    printField("tpt", tree.tpt)
    printField("rhs", tree.rhs)
  }




  //  case class TypeDef[-T >: Untyped] private[ast] (name: TypeName, rhs: Tree[T])
  def dumpTypeDef(tree: TypeDef): Unit = {
    printString("name", tree.name.toString)
    printField("rhs", tree.rhs)
  }


  // case class ValDef[-T >: Untyped] private[ast] (ValDef: TermName, tpt: Tree[T], rhs: Tree[T])
  def dumpValDef(tree: ValDef) = {
    printString("name", tree.name.toString)
    printField("tpt", tree.tpt)
    printField("rhs", tree.rhs)
  }

  //  case class Template[-T >: Untyped] private[ast] (constr: DefDef[T], parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]])
  def dumpTemplate(tree: Template) = {
    printField("constr", tree.constr)
    printListField("parents", tree.parents)
    printField("self", tree.self)
    printListField("body", tree.body)
    }


  //  case class Apply[-T >: Untyped] private[ast] (fun: Tree[T], args: List[Tree[T]])
  def dumpApply(tree: Apply): Unit = {
    printField("fun", tree.fun)
    printListField("args", tree.args)
  }


}
