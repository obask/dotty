package dotty.tools.backend.ssa

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Types

import scala.collection.mutable


object Lispyfy {

  protected def initCtx = (new ContextBase).initialCtx

  implicit val ctx: Context = initCtx.fresh

  def dispatch(ast: tpd.Tree) = {
    ast match {
      case tree: tpd.DefDef =>
        process(tree)
      case tree: tpd.ValDef =>
        process(tree)
      case tree: tpd.Template =>
        process(tree)
      case _ => expression(ast)
    }
  }

  def expression(tree: tpd.Tree): Unit = {
    tree match {
      case xx: Block => procBlock(xx)
      case xx: New => process(xx)
      case xx: If => procIf(xx)
      case xx: Apply => process(xx)
      case xx: Assign => procAssign(xx.lhs, xx.rhs)
      case xx: Ident => debug(xx.name)
      case xx: Select => process(xx)
      case xx: Match => procMatch(xx)
      case xx: Literal => xx.const match {
        case vv: Constant if vv.isNumeric => debug(vv.stringValue)
        case vv: Constant if !vv.isNumeric => debug("\"" + vv.stringValue + "\"")
      }
      case xx: Super => debug("(Super " + xx.qual.toString + ")")
      case xx: This => debug("(This " + xx.qual.toString + ")")
      case EmptyTree => debug("EmptyTree")
    }
  }



  def procXZ(tp: Types.Type): Unit = {
    tp match {
      // abstract case class TypeRef(override val prefix: Type, name: TypeName) extends NamedType {
      case xx: Types.TypeRef =>
        print(xx.name )
      // abstract case class JavaArrayType(elemType: Type) extends CachedGroundType with ValueType {
      case xx: Types.JavaArrayType =>
        print("(Array ")
        procXZ(xx.elemType)
        print(" )")
    }

  }

//  val typesMap = mutable.HashMap[String, String]()

  def dispatchType(tree: tpd.Tree) = {
    tree match {
      // case class TypeTree[-T >: Untyped] private[ast] (original: Tree[T])
      case x: tpd.TypeTree =>
        // FIXME magic happens with type ref
        if (x.hasType)
          procXZ(x.typeOpt)
        else
          print(x.original)
      case xx: Ident =>
        debug(xx.name.toString)
      // case class AppliedTypeTree[-T >: Untyped] private[ast] (tpt: Tree[T], args: List[Tree[T]])
      case xx: AppliedTypeTree =>
        processType(xx)
      case EmptyTree =>
        debug("EmptyTree")
    }
  }


  def processType(xx: AppliedTypeTree): Unit = {
    print("(")
    print(xx.tpt match {case x: Ident => x.name})
    print(" ")
    val s = (xx.args.head match {case x: Ident => x.name}).toString
    print(s)
    print(") ")
  }




  // case class PackageDef[-T >: Untyped] private[ast] (pid: RefTree[T], stats: List[Tree[T]])
  def processPackageDef(tree: tpd.PackageDef) {
    debug("(PackageDef ")
    shiftLn()
    for(st <- tree.stats) {
      st match {
        case xx: TypeDef =>
          processTypeDef(xx)
        case xx: ValDef =>
          process(xx)
      }
      shiftLn()
    }
    debug(")")
    shiftLn()
  }

  // TODO make type signature

  //  case class TypeDef[-T >: Untyped] private[ast] (name: TypeName, rhs: Tree[T])
  def processTypeDef(tree: TypeDef): Unit = {
    shiftLn()
    debug("(TypeDef " + tree.name )
    tree.rhs match {
      // case class Template[-T >: Untyped] private[ast] (constr: DefDef[T], parents: List[Tree[T]], self: ValDef[T], body: List[Tree[T]])
      case xx: Template => {
        process(xx)
      }
    }
    debug(")")
    shiftLn()
  }

//  case class Template[-T >: Untyped] private[ast] (constr: DefDef[T], parents: List[Tree[T]], self: ValDef[T], private var preBody: LazyTreeList)
  def process(tree: tpd.Template) {
    shiftRight()
    shiftLn()
    debug("(extends")
    for(parent <- tree.parents) {
      dispatchType(parent)
    }
    debug(")")
    shiftLn()
    process(tree.constr)
    for (tmp <- tree.body) {
      dispatch(tmp)
    }
    shiftLeft()
    shiftLn()
  }


  // case class DefDef(name: TermName, tparams: List[TypeDef[T]], vparamss: List[List[ValDef[T]]], tpt: Tree[T], rhs: Tree[T])
  def process(tree: DefDef): Unit = {
    procTypeSignature(tree)

    if (tree.rhs match {
      case xx: Ident => xx.name.toString == "???"
      case _ => false}) {
      return
    }

    //    println("/* DEBUG: " + tree.vparamss + " */") // U dont need this
    //    println("fun vparams: " + tree.vparamss)
    //        println("/* fun tpt: " + tree.tpt + "*/")
    shiftLn()
    debug("(defun " + tree.name)
    procParamsList(tree.vparamss)
    shiftRight()
    shiftLn()
    //  println("fun rhs: " + tree.rhs) // body block
    dispatch(tree.rhs)
    //    procBlock(.asInstanceOf[Block])
    print(")")
    shiftLeft()
    shiftLn()
  }

  // case class ValDef[-T >: Untyped] private[ast] (name: TermName, tpt: Tree[T], rhs: Tree[T])
  def process(tree: ValDef) = {
    debug("(ValDef " + tree.name)
    debug(":")
    dispatchType(tree.tpt)
    debug(" =")
    expression(tree.rhs)
    debug(")")
    shiftLn()
  }

  //  case class Apply[-T >: Untyped] private[ast] (fun: Tree[T], args: List[Tree[T]])
  def process(tree: Apply): Unit = {
    // Select(New(AppliedTypeTree(Ident(Array),List(Ident(Int)))),<init>)
    tree.fun match {
      case Trees.Select(Trees.New(Trees.AppliedTypeTree(Trees.Ident(xARRAY), List(xELEMS))), _) =>
        assert(xARRAY.toString == "Array")
        print("(ArrayCreate ")
        val s = xELEMS.asInstanceOf[Ident].name.toString
        print(s + " ")
        expression(tree.args.head)
        print(")")
      case xx: Select =>
        debug("(ApplySelect")
        expression(xx.qualifier)
        debug(xx.name)
        for (arg <- tree.args) {
          expression(arg)
        }
        debug(")")
      case xx: Ident =>
        debug("(Apply ")
        debug(xx.name)
        for (arg <- tree.args) {
          expression(arg)
        }
        debug(")")
    }
  }


  // case class Select[-T >: Untyped] private[ast] (qualifier: Tree[T], name: Name)
  def process(tree: Select): Unit = {
    debug("(Select")
    expression(tree.qualifier)
    debug(tree.name)
    debug(")")
  }

  // case class New[-T >: Untyped] private[ast] (tpt: Tree[T])
  def process(tree: tpd.New) {
    debug("(New ")

    dispatchType(tree.tpt)


    debug(")")
  }
















  var shift = 0


  def debug(tree: Any) = {
    print(tree + " ")
  }
  def shiftLn(): Unit = {
    println()
    print(" " * shift)
  }

  def shiftLeft(): Unit = {
    shift -= 4
  }

  def shiftRight(): Unit = {
    shift += 4
  }


  //  case class If[-T >: Untyped] private[ast] (cond: Tree[T], thenp: Tree[T], elsep: Tree[T])
  def procIf(tree: If) = {
    debug("(If")
    expression(tree.cond)
    shiftRight()
    shiftLn()
    expression(tree.thenp)
    shiftLn()
    print("Else")
    shiftLn()
    expression(tree.elsep)
    shiftLeft()
    debug(")")
  }



  // case class Block[-T >: Untyped] private[ast] (stats: List[Tree[T]], expr: Tree[T])
  def procBlock(xx: Block) = {
    if (xx.stats.nonEmpty) {
      debug("(Block")
//      print(s"/*  $shift */")
      shiftRight()
      for (el <- xx.stats) {
        shiftLn()
        expression(el)
      }
      shiftLn()
      expression(xx.expr)
      shiftLeft()
      debug(")")
    } else {
      expression(xx.expr)
    }
  }

  //  case class InfixOp(left: Tree, op: Name, right: Tree) extends OpTree
//  def procInfixOp(tree: InfixOp) = {
//    debug("(" + tree.op)
//    shiftRight()
//    expression(tree.left)
//    expression(tree.right)
//    shiftLeft()
//    debug(")")
//  }


  def dbg(x: Tree) = {
    print("/* " + x + " */")
  }

//  type Untyped = Null






//  case class DefDef[-T >: Untyped] private[ast] (name: TermName, tparams: List[TypeDef[T]],
//                                                 vparamss: List[List[ValDef[T]]], tpt: Tree[T], private var preRhs: LazyTree)
//  def procFunction(tree: tpd.DefDef) = {
//    shiftLn()
//    debug("(Function (")
//    for (param <- tree.args) {
//      param match {
//        case xx: ValDef => debug(xx.name)
//        case _ => throw new Exception("procDefDef bad function param")
//      }
//    }
//    debug(")")
//      shiftRight()
//      shiftLn()
//      expression(tree.body)
//      shiftLeft()
//      shiftLn()
//      print(")")
//
//  }

//  case class CaseDef[-T >: Untyped] private[ast] (pat: Tree[T], guard: Tree[T], body: Tree[T])
  def procCaseDef(tree: CaseDef) = {
    debug("(case ")
  tree.pat match {
    case xx: Typed => procTyped(xx)
    case xx: Ident => debug(xx.name)
  }
    shiftLn()
    expression(tree.body)
    debug(")")
    shiftLn()
}

//  case class Match[-T >: Untyped] private[ast] (selector: Tree[T], cases: List[CaseDef[T]])
  def procMatch(tree: Match) = {
    shiftLn()
    debug("(Match ")
    expression(tree.selector)
    shiftRight()
    shiftLn()
    for (caseDef <- tree.cases) {
      procCaseDef(caseDef)
    }
    debug(")")
    shiftLeft()
    shiftLn()
}

//  case class Typed[-T >: Untyped] private[ast] (expr: Tree[T], tpt: Tree[T])
  def procTyped(tree: Typed) = {
    debug("(isInstanceOf")
    dispatchType(tree.tpt)
    expression(tree.expr)
    debug(")")
  }


  def procAssign(lhs: Tree, rhs: Tree) = {
    // Assign(Apply(Ident(tt),List(Literal(Constant(2)))),   Literal(Constant(1)))
    lhs match {
      case Trees.Apply(Trees.Ident(mVAR), List(mPOS)) =>
        print("(ArrayAssign " + mVAR + " ")
        expression(mPOS)
//        print("SET! ")
        expression(rhs)
        print(")")
      case xx: Ident => {
        print("(Assign " + xx.name + " ")
        expression(rhs)
        print(")")
      }
      case xx: Select => {
        print("(AssignSelect " + xx.name + " ")
        expression(rhs)
        print(")")
      }
    }

  }




  // case class ValDef[-T >: Untyped] private[ast] (name: TermName, tpt: Tree[T], rhs: Tree[T])
//  def procFunctionArgument(arg: ValDef): Unit = {
//    print("(TyVal ")
//    debug(arg.name)
//    arg.tpt match {case xx: Ident => print(xx.name)}
//    print(") ")
//  }


  def procParamsList(vparamss: List[List[ValDef]]) = {
    print("(")
    for (arg <- vparamss) {
      for (param <- arg) {
        param match {
          case xx: ValDef => debug(xx.name)
          case _ => throw new Exception("procDefDef bad function param")
        }
      }
    }
    print(") ")
  }



  def procTypeSignature(tree: DefDef) = {
    //    showDate :: Int -> Int -> Int -> String
    shiftLn()
    debug("(Signature " + tree.name + " ")
    for (arg <- tree.vparamss) {
      print("(# ")
      for (param <- arg) {
        param match {
          case xx: ValDef =>
            dispatchType(xx.tpt)
        }
      }
      print(")")
    }
    print(" -> ")
    dispatchType(tree.tpt)
    print(")")
//    shiftLeft()
  }




//  def procObjectDef(modulo: ModuleDef) {
//    shiftLn()
//    debug("(ModuleDef " + modulo.name )
//    procTemplate(modulo.impl)
//    debug(")")
//    shiftLn()
//  }


}
