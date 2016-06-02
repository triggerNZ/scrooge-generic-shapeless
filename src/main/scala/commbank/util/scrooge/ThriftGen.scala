package commbank.util.scrooge

import scala.language.experimental.macros

import scala.reflect.macros.whitebox

import shapeless._
import com.twitter.scrooge.ThriftStruct

object ThriftGen {
  def apply[T <: ThriftStruct](implicit gen: Generic[T]): Generic.Aux[T, gen.Repr] = gen
  implicit def thriftGen[T <: ThriftStruct, R]: Generic.Aux[T, R] = macro ThriftGenericMacros.materialize[T, R]
}

@macrocompat.bundle
class ThriftGenericMacros(val c: whitebox.Context) extends CaseClassMacros {
  import c.universe._
  import internal.constantType
  import Flag._



  def thriftStructTpe = typeOf[ThriftStruct]

  def materialize[T: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    if (!isScroogeType(tpe)) {
      abort(s"$tpe is not a scrooge struct")
    }

    mkProductThrift(tpe)

  }

  def nestedImmutable(tpe: Type): Type = {
    val companionType = tpe.companion
    val immutableType = companionType.member(TypeName("Immutable")).asType.toType
    immutableType
  }


  def mkProductThrift(tpe: Type): Tree = {
    val ctorDtor = ThriftCtorDtor(nestedImmutable(tpe))
    val (p, ts) = ctorDtor.binding

    val to = cq""" $p => ${mkHListValue(ts)} """
    val (rp, rts) = ctorDtor.reprBinding
    val from = cq""" $rp => ${ctorDtor.construct(rts)} """

    val reprType = scroogeReprTypeTree(tpe)

    val clsName = TypeName(c.freshName("anon$"))


    val output = q"""
      final class $clsName extends _root_.shapeless.Generic[$tpe] {
        type Repr = ${reprType}
        def to(p: $tpe): Repr = (p match { case $to }).asInstanceOf[Repr]
        def from(p: Repr): $tpe = p match { case $from }
      }
      new $clsName(): _root_.shapeless.Generic.Aux[$tpe, ${reprType}]
    """
    output
  }

  object ThriftCtorDtor {
    def apply(tpe: Type): CtorDtor = {
      def narrow(tree: Tree, tpe: Type): Tree =
        tpe match {
          case ConstantType(c) =>
            q"$c.asInstanceOf[$tpe]"
          case _ =>
            tree
        }

      def narrow1(tree: Tree, tpe: Type): Tree =
        if(isVararg(tpe))
          q"$tree: _*"
        else
          narrow(tree, tpe)


      def mkCtorDtor1(elems: List[(TermName, TermName, Type)], pattern: Tree, rhs: List[Tree]) = {
        val reprPattern =
          elems.foldRight(q"_root_.shapeless.HNil": Tree) {
            case ((bound, _, _), acc) => pq"_root_.shapeless.::($bound, $acc)"
          }
        new CtorDtor {
          def construct(args: List[Tree]): Tree = q"new $tpe(..$args)"
          def binding: (Tree, List[Tree]) = (pattern, rhs)
          def reprBinding: (Tree, List[Tree]) = (reprPattern, elems.map { case (binder, _, tpe) => narrow1(q"$binder", tpe) })
        }
      }

      lowerKind(tpe) match {
        // case 6: concrete, exactly one public constructor with matching accessible fields
        case HasUniqueCtor(args) =>
          val elems = args.map { case (name, tpe) => (TermName(c.freshName("pat")), name, tpe) }.filter(_._2.toString != "_passthroughFields")
          val binder = TermName(c.freshName("pat"))
          val pattern = pq"$binder"
          val rhs = elems.map { case (_, name, tpe) => narrow(q"$binder.$name", tpe) }
          mkCtorDtor1(elems, pattern, rhs)

        case _ => abort(s"Bad product type $tpe")
      }
    }
  }

  def scroogeReprTypeTree(tpe: Type): Tree = {
    mkCompoundTypTree(typeOf[shapeless.HNil], typeOf[shapeless.::[_,_]].typeConstructor, fieldsOf(nestedImmutable(tpe)).filter(_._1.toString != "_passthroughFields").map(_._2))
  }

  def isScroogeType(tpe: Type): Boolean = {
    val sym = classSym(tpe)
    tpe <:< thriftStructTpe && sym.isTrait
  }
}
