package dotty.tools
package dotc
package core

import Contexts._, Types._, Symbols._, Names._, Flags._, Scopes._
import SymDenotations._, Denotations.SingleDenotation
import config.Printers.typr
import util.Positions._
import NameOps._
import Decorators._
import StdNames._
import Annotations._
import util.SimpleMap
import collection.mutable
import ast.tpd._

trait TypeOps { this: Context => // TODO: Make standalone object.

  /** The type `tp` as seen from prefix `pre` and owner `cls`. See the spec
   *  for what this means. Called very often, so the code is optimized heavily.
   *
   *  A tricky aspect is what to do with unstable prefixes. E.g. say we have a class
   *
   *    class C { type T; def f(x: T): T }
   *
   *  and an expression `e` of type `C`. Then computing the type of `e.f` leads
   *  to the query asSeenFrom(`C`, `(x: T)T`). What should its result be? The
   *  naive answer `(x: C#T)C#T` is incorrect given that we treat `C#T` as the existential
   *  `exists(c: C)c.T`. What we need to do instead is to skolemize the existential. So
   *  the answer would be `(x: c.T)c.T` for some (unknown) value `c` of type `C`.
   *  `c.T` is expressed in the compiler as a skolem type `Skolem(C)`.
   *
   *  Now, skolemization is messy and expensive, so we want to do it only if we absolutely
   *  must. Also, skolemizing immediately would mean that asSeenFrom was no longer
   *  idempotent - each call would return a type with a different skolem.
   *  Instead we produce an annotated type that marks the prefix as unsafe:
   *
   *     (x: (C @ UnsafeNonvariant)#T)C#T
   *
   *  We also set a global state flag `unsafeNonvariant` to the current run.
   *  When typing a Select node, typer will check that flag, and if it
   *  points to the current run will scan the result type of the select for
   *  @UnsafeNonvariant annotations. If it finds any, it will introduce a skolem
   *  constant for the prefix and try again.
   *
   *  The scheme is efficient in particular because we expect that unsafe situations are rare;
   *  most compiles would contain none, so no scanning would be necessary.
   */
  final def asSeenFrom(tp: Type, pre: Type, cls: Symbol): Type =
    asSeenFrom(tp, pre, cls, null)

  /** Helper method, taking a map argument which is instantiated only for more
   *  complicated cases of asSeenFrom.
   */
  private def asSeenFrom(tp: Type, pre: Type, cls: Symbol, theMap: AsSeenFromMap): Type = {

    /** Map a `C.this` type to the right prefix. If the prefix is unstable and
     *  the `C.this` occurs in nonvariant or contravariant position, mark the map
     *  to be unstable.
     */
    def toPrefix(pre: Type, cls: Symbol, thiscls: ClassSymbol): Type = /*>|>*/ ctx.conditionalTraceIndented(TypeOps.track, s"toPrefix($pre, $cls, $thiscls)") /*<|<*/ {
      if ((pre eq NoType) || (pre eq NoPrefix) || (cls is PackageClass))
        tp
      else pre match {
        case pre: SuperType => toPrefix(pre.thistpe, cls, thiscls)
        case _ =>
          if (thiscls.derivesFrom(cls) && pre.baseTypeRef(thiscls).exists) {
            if (theMap != null && theMap.currentVariance <= 0 && !isLegalPrefix(pre)) {
              ctx.base.unsafeNonvariant = ctx.runId
              AnnotatedType(pre, Annotation(defn.UnsafeNonvariantAnnot, Nil))
            }
            else pre
          }
          else if ((pre.termSymbol is Package) && !(thiscls is Package))
            toPrefix(pre.select(nme.PACKAGE), cls, thiscls)
          else
            toPrefix(pre.baseTypeRef(cls).normalizedPrefix, cls.owner, thiscls)
      }
    }

    /*>|>*/ ctx.conditionalTraceIndented(TypeOps.track, s"asSeen ${tp.show} from (${pre.show}, ${cls.show})", show = true) /*<|<*/ { // !!! DEBUG
      tp match {
        case tp: NamedType =>
          val sym = tp.symbol
          if (sym.isStatic) tp
          else {
            val pre1 = asSeenFrom(tp.prefix, pre, cls, theMap)
            if (pre1.isUnsafeNonvariant)
              pre1.member(tp.name).info match {
                case TypeAlias(alias) =>
                  // try to follow aliases of this will avoid skolemization.
                  return alias
                case _ =>
              }
            tp.derivedSelect(pre1)
          }
        case tp: ThisType =>
          toPrefix(pre, cls, tp.cls)
        case _: BoundType | NoPrefix =>
          tp
        case tp: RefinedType =>
          tp.derivedRefinedType(
            asSeenFrom(tp.parent, pre, cls, theMap),
            tp.refinedName,
            asSeenFrom(tp.refinedInfo, pre, cls, theMap))
        case tp: TypeAlias if tp.variance == 1 => // if variance != 1, need to do the variance calculation
          tp.derivedTypeAlias(asSeenFrom(tp.alias, pre, cls, theMap))
        case _ =>
          (if (theMap != null) theMap else new AsSeenFromMap(pre, cls))
            .mapOver(tp)
      }
    }
  }

  private def isLegalPrefix(pre: Type)(implicit ctx: Context) =
    pre.isStable || !ctx.phase.isTyper

  /** The TypeMap handling the asSeenFrom in more complicated cases */
  class AsSeenFromMap(pre: Type, cls: Symbol) extends TypeMap {
    def apply(tp: Type) = asSeenFrom(tp, pre, cls, this)

    /** A method to export the current variance of the map */
    def currentVariance = variance
  }

  /** Approximate a type `tp` with a type that does not contain skolem types. */
  object deskolemize extends ApproximatingTypeMap {
    private var seen: Set[SkolemType] = Set()
    def apply(tp: Type) = tp match {
      case tp: SkolemType =>
        if (seen contains tp) NoType
        else {
          val saved = seen
          seen += tp
          try approx(hi = tp.info)
          finally seen = saved
        }
      case _ =>
        mapOver(tp)
    }
  }

  /** Implementation of Types#simplified */
  final def simplify(tp: Type, theMap: SimplifyMap): Type = tp match {
    case tp: NamedType =>
      if (tp.symbol.isStatic) tp
      else tp.derivedSelect(simplify(tp.prefix, theMap)) match {
        case tp1: NamedType if tp1.denotationIsCurrent =>
          val tp2 = tp1.reduceProjection
          //if (tp2 ne tp1) println(i"simplified $tp1 -> $tp2")
          tp2
        case tp1 => tp1
      }
    case tp: PolyParam =>
      typerState.constraint.typeVarOfParam(tp) orElse tp
    case  _: ThisType | _: BoundType | NoPrefix =>
      tp
    case tp: RefinedType =>
      tp.derivedRefinedType(simplify(tp.parent, theMap), tp.refinedName, simplify(tp.refinedInfo, theMap))
    case tp: TypeAlias =>
      tp.derivedTypeAlias(simplify(tp.alias, theMap))
    case AndType(l, r) =>
      simplify(l, theMap) & simplify(r, theMap)
    case OrType(l, r) =>
      simplify(l, theMap) | simplify(r, theMap)
    case _ =>
      (if (theMap != null) theMap else new SimplifyMap).mapOver(tp)
  }

  class SimplifyMap extends TypeMap {
    def apply(tp: Type) = simplify(tp, this)
  }

  /** Approximate union type by intersection of its dominators.
   *  That is, replace a union type Tn | ... | Tn
   *  by the smallest intersection type of base-class instances of T1,...,Tn.
   *  Example: Given
   *
   *      trait C[+T]
   *      trait D
   *      class A extends C[A] with D
   *      class B extends C[B] with D with E
   *
   *  we approximate `A | B` by `C[A | B] with D`
   */
  def orDominator(tp: Type): Type = {

    /** a faster version of cs1 intersect cs2 */
    def intersect(cs1: List[ClassSymbol], cs2: List[ClassSymbol]): List[ClassSymbol] = {
      val cs2AsSet = new util.HashSet[ClassSymbol](100)
      cs2.foreach(cs2AsSet.addEntry)
      cs1.filter(cs2AsSet.contains)
    }

    /** The minimal set of classes in `cs` which derive all other classes in `cs` */
    def dominators(cs: List[ClassSymbol], accu: List[ClassSymbol]): List[ClassSymbol] = (cs: @unchecked) match {
      case c :: rest =>
        val accu1 = if (accu exists (_ derivesFrom c)) accu else c :: accu
        if (cs == c.baseClasses) accu1 else dominators(rest, accu1)
      case Nil => // this case can happen because after erasure we do not have a top class anymore
        assert(ctx.erasedTypes)
        defn.ObjectClass :: Nil
    }

    def mergeRefined(tp1: Type, tp2: Type): Type = {
      def fail = throw new AssertionError(i"Failure to join alternatives $tp1 and $tp2")
      tp1 match {
        case tp1 @ RefinedType(parent1, name1, rinfo1) =>
          tp2 match {
            case RefinedType(parent2, `name1`, rinfo2) =>
              tp1.derivedRefinedType(
                mergeRefined(parent1, parent2), name1, rinfo1 | rinfo2)
            case _ => fail
          }
        case tp1 @ TypeRef(pre1, name1) =>
          tp2 match {
            case tp2 @ TypeRef(pre2, `name1`) =>
              tp1.derivedSelect(pre1 | pre2)
            case _ => fail
          }
        case _ => fail
      }
    }

    def approximateOr(tp1: Type, tp2: Type): Type = {
      def isClassRef(tp: Type): Boolean = tp match {
        case tp: TypeRef => tp.symbol.isClass
        case tp: RefinedType => isClassRef(tp.parent)
        case _ => false
      }

      tp1 match {
        case tp1: RecType =>
          tp1.rebind(approximateOr(tp1.parent, tp2))
        case tp1: TypeProxy if !isClassRef(tp1) =>
          orDominator(tp1.superType | tp2)
        case _ =>
          tp2 match {
            case tp2: RecType =>
              tp2.rebind(approximateOr(tp1, tp2.parent))
            case tp2: TypeProxy if !isClassRef(tp2) =>
              orDominator(tp1 | tp2.superType)
            case _ =>
              val commonBaseClasses = tp.mapReduceOr(_.baseClasses)(intersect)
              val doms = dominators(commonBaseClasses, Nil)
              def baseTp(cls: ClassSymbol): Type = {
                val base =
                  if (tp1.typeParams.nonEmpty) tp.baseTypeRef(cls)
                  else tp.baseTypeWithArgs(cls)
                base.mapReduceOr(identity)(mergeRefined)
              }
              doms.map(baseTp).reduceLeft(AndType.apply)
          }
      }
    }

    tp match {
      case tp: OrType =>
        approximateOr(tp.tp1, tp.tp2)
      case _ =>
        tp
    }
  }

  /** Given a disjunction T1 | ... | Tn of types with potentially embedded
   *  type variables, constrain type variables further if this eliminates
   *  some of the branches of the disjunction. Do this also for disjunctions
   *  embedded in intersections, as parents in refinements, and in recursive types.
   *
   *  For instance, if `A` is an unconstrained type variable, then
   *
   *      ArrayBuffer[Int] | ArrayBuffer[A]
   *
   *  is approximated by constraining `A` to be =:= to `Int` and returning `ArrayBuffer[Int]`
   *  instead of `ArrayBuffer[_ >: Int | A <: Int & A]`
   */
  def harmonizeUnion(tp: Type): Type = tp match {
    case tp: OrType =>
      joinIfScala2(typeComparer.fluidly(tp.tp1 | tp.tp2))
    case tp @ AndType(tp1, tp2) =>
      tp derived_& (harmonizeUnion(tp1), harmonizeUnion(tp2))
    case tp: RefinedType =>
      tp.derivedRefinedType(harmonizeUnion(tp.parent), tp.refinedName, tp.refinedInfo)
    case tp: RecType =>
      tp.rebind(harmonizeUnion(tp.parent))
    case _ =>
      tp
  }

  /** Under -language:Scala2: Replace or-types with their joins */
  private def joinIfScala2(tp: Type) = tp match {
    case tp: OrType if scala2Mode => tp.join
    case _ => tp
  }

  /** Not currently needed:
   *
  def liftToRec(f: (Type, Type) => Type)(tp1: Type, tp2: Type)(implicit ctx: Context) = {
    def f2(tp1: Type, tp2: Type): Type = tp2 match {
      case tp2: RecType => tp2.rebind(f(tp1, tp2.parent))
      case _ => f(tp1, tp2)
    }
    tp1 match {
      case tp1: RecType => tp1.rebind(f2(tp1.parent, tp2))
      case _ => f2(tp1, tp2)
    }
  }
  */

  private def enterArgBinding(formal: Symbol, info: Type, cls: ClassSymbol, decls: Scope) = {
    val lazyInfo = new LazyType { // needed so we do not force `formal`.
      def complete(denot: SymDenotation)(implicit ctx: Context): Unit = {
        denot setFlag formal.flags & RetainedTypeArgFlags
        denot.info = info
      }
    }
    val sym = ctx.newSymbol(
      cls, formal.name,
      formal.flagsUNSAFE & RetainedTypeArgFlags | BaseTypeArg | Override,
      lazyInfo,
      coord = cls.coord)
    cls.enter(sym, decls)
  }

  /** If `tpe` is of the form `p.x` where `p` refers to a package
   *  but `x` is not owned by a package, expand it to
   *
   *      p.package.x
   */
  def makePackageObjPrefixExplicit(tpe: NamedType): Type = {
    def tryInsert(pkgClass: SymDenotation): Type = pkgClass match {
      case pkgCls: PackageClassDenotation if !(tpe.symbol.maybeOwner is Package) =>
        tpe.derivedSelect(pkgCls.packageObj.valRef)
      case _ =>
        tpe
    }
    tpe.prefix match {
      case pre: ThisType if pre.cls is Package => tryInsert(pre.cls)
      case pre: TermRef if pre.symbol is Package => tryInsert(pre.symbol.moduleClass)
      case _ => tpe
    }
  }

  /** If we have member definitions
   *
   *     type argSym v= from
   *     type from v= to
   *
   *  where the variances of both alias are the same, then enter a new definition
   *
   *     type argSym v= to
   *
   *  unless a definition for `argSym` already exists in the current scope.
   */
  def forwardRef(argSym: Symbol, from: Symbol, to: TypeBounds, cls: ClassSymbol, decls: Scope) =
    argSym.info match {
      case info @ TypeBounds(lo2 @ TypeRef(_: ThisType, name), hi2) =>
        if (name == from.name &&
            (lo2 eq hi2) &&
            info.variance == to.variance &&
            !decls.lookup(argSym.name).exists) {
              // println(s"short-circuit ${argSym.name} was: ${argSym.info}, now: $to")
              enterArgBinding(argSym, to, cls, decls)
            }
      case _ =>
    }


  /** Normalize a list of parent types of class `cls` that may contain refinements
   *  to a list of typerefs referring to classes, by converting all refinements to member
   *  definitions in scope `decls`. Can add members to `decls` as a side-effect.
   */
  def normalizeToClassRefs(parents: List[Type], cls: ClassSymbol, decls: Scope): List[TypeRef] = {

    /** If we just entered the type argument binding
     *
     *    type From = To
     *
     *  and there is a type argument binding in a parent in `prefs` of the form
     *
     *    type X = From
     *
     *  then also add the binding
     *
     *    type X = To
     *
     *  to the current scope, provided (1) variances of both aliases are the same, and
     *  (2) X is not yet defined in current scope. This "short-circuiting" prevents
     *  long chains of aliases which would have to be traversed in type comparers.
     *
     *  Note: Test i1401.scala shows that `forwardRefs` is also necessary
     *  for typechecking in the case where self types refer to type parameters
     *  that are upper-bounded by subclass instances.
     */
    def forwardRefs(from: Symbol, to: Type, prefs: List[TypeRef]) = to match {
      case to @ TypeBounds(lo1, hi1) if lo1 eq hi1 =>
        for (pref <- prefs) {
          def forward(): Unit =
            for (argSym <- pref.decls)
              if (argSym is BaseTypeArg)
                forwardRef(argSym, from, to, cls, decls)
          pref.info match {
            case info: TempClassInfo => info.addSuspension(forward)
            case _ => forward()
          }
        }
      case _ =>
    }

    // println(s"normalizing $parents of $cls in ${cls.owner}") // !!! DEBUG

    // A map consolidating all refinements arising from parent type parameters
    var refinements: SimpleMap[TypeName, Type] = SimpleMap.Empty

    // A map of all formal type parameters of base classes that get refined
    var formals: SimpleMap[TypeName, Symbol] = SimpleMap.Empty // A map of all formal parent parameter

    // Strip all refinements from parent type, populating `refinements` and `formals` maps.
    def normalizeToRef(tp: Type): TypeRef = {
      def fail = throw new TypeError(s"unexpected parent type: $tp")
      tp.dealias match {
        case tp: TypeRef =>
          tp
        case tp @ RefinedType(tp1, name: TypeName, rinfo) =>
          rinfo match {
            case TypeAlias(TypeRef(pre, name1)) if name1 == name && (pre =:= cls.thisType) =>
              // Don't record refinements of the form X = this.X (These can arise using named parameters).
              typr.println(s"dropping refinement $tp")
            case _ =>
              val prevInfo = refinements(name)
              refinements = refinements.updated(name,
                if (prevInfo == null) tp.refinedInfo else prevInfo & tp.refinedInfo)
              formals = formals.updated(name, tp1.typeParamNamed(name))
          }
          normalizeToRef(tp1)
        case ErrorType =>
          defn.AnyType
        case AnnotatedType(tpe, _) =>
          normalizeToRef(tpe)
        case HKApply(tycon: TypeRef, args) =>
          tycon.info match {
            case TypeAlias(alias) => normalizeToRef(alias.appliedTo(args))
            case _ => fail
          }
        case _ =>
          fail
      }
    }

    val parentRefs = parents map normalizeToRef

    // Enter all refinements into current scope.
    refinements foreachBinding { (name, refinedInfo) =>
      assert(decls.lookup(name) == NoSymbol, // DEBUG
        s"redefinition of ${decls.lookup(name).debugString} in ${cls.showLocated}")
      enterArgBinding(formals(name), refinedInfo, cls, decls)
    }
    // Forward definitions in super classes that have one of the refined parameters
    // as aliases directly to the refined info.
    // Note that this cannot be fused with the previous loop because we now
    // assume that all arguments have been entered in `decls`.
    refinements foreachBinding { (name, refinedInfo) =>
      forwardRefs(formals(name), refinedInfo, parentRefs)
    }
    parentRefs
  }

  /** An argument bounds violation is a triple consisting of
   *   - the argument tree
   *   - a string "upper" or "lower" indicating which bound is violated
   *   - the violated bound
   */
  type BoundsViolation = (Tree, String, Type)

  /** The list of violations where arguments are not within bounds.
   *  @param  args          The arguments
   *  @param  boundss       The list of type bounds
   *  @param  instantiate   A function that maps a bound type and the list of argument types to a resulting type.
   *                        Needed to handle bounds that refer to other bounds.
   */
  def boundsViolations(args: List[Tree], boundss: List[TypeBounds], instantiate: (Type, List[Type]) => Type)(implicit ctx: Context): List[BoundsViolation] = {
    val argTypes = args.tpes
    val violations = new mutable.ListBuffer[BoundsViolation]
    for ((arg, bounds) <- args zip boundss) {
      def checkOverlapsBounds(lo: Type, hi: Type): Unit = {
        //println(i"instantiating ${bounds.hi} with $argTypes")
        //println(i" = ${instantiate(bounds.hi, argTypes)}")
        val hiBound = instantiate(bounds.hi, argTypes.mapConserve(_.bounds.hi))
        val loBound = instantiate(bounds.lo, argTypes.mapConserve(_.bounds.lo))
          // Note that argTypes can contain a TypeBounds type for arguments that are
          // not fully determined. In that case we need to check against the hi bound of the argument.
        if (!(lo <:< hiBound)) violations += ((arg, "upper", hiBound))
        if (!(loBound <:< hi)) violations += ((arg, "lower", bounds.lo))
      }
      arg.tpe match {
        case TypeBounds(lo, hi) => checkOverlapsBounds(lo, hi)
        case tp => checkOverlapsBounds(tp, tp)
      }
    }
    violations.toList
  }

  /** Is `feature` enabled in class `owner`?
   *  This is the case if one of the following two alternatives holds:
   *
   *  1. The feature is imported by a named import
   *
   *       import owner.feature
   *
   *  (the feature may be bunched with others, or renamed, but wildcard imports
   *  don't count).
   *
   *  2. The feature is enabled by a compiler option
   *
   *       - language:<prefix>feature
   *
   *  where <prefix> is the full name of the owner followed by a "." minus
   *  the prefix "dotty.language.".
   */
  def featureEnabled(owner: ClassSymbol, feature: TermName): Boolean = {
    def toPrefix(sym: Symbol): String =
      if (!sym.exists || (sym eq defn.LanguageModuleClass)) ""
      else toPrefix(sym.owner) + sym.name + "."
    def featureName = toPrefix(owner) + feature
    def hasImport(implicit ctx: Context): Boolean = {
      if (ctx.importInfo == null || (ctx.importInfo.site.widen.typeSymbol ne owner)) false
      else if (ctx.importInfo.excluded.contains(feature)) false
      else if (ctx.importInfo.originals.contains(feature)) true
      else {
        var c = ctx.outer
        while (c.importInfo eq ctx.importInfo) c = c.outer
        hasImport(c)
      }
    }
    def hasOption = ctx.base.settings.language.value exists (s => s == featureName || s == "_")
    hasImport(ctx.withPhase(ctx.typerPhase)) || hasOption
  }

  /** Is auto-tupling enabled? */
  def canAutoTuple =
    !featureEnabled(defn.LanguageModuleClass, nme.noAutoTupling)

  def scala2Mode =
    featureEnabled(defn.LanguageModuleClass, nme.Scala2)

  def dynamicsEnabled =
    featureEnabled(defn.LanguageModuleClass, nme.dynamics)

  def testScala2Mode(msg: String, pos: Position) = {
    if (scala2Mode) migrationWarning(msg, pos)
    scala2Mode
  }
}

object TypeOps {
  @sharable var track = false // !!!DEBUG
}
