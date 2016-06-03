package commbank.util.scrooge

import shapeless._, test.illTyped
import org.specs2._

import thrift.Demo

object ThriftGenSpec extends Specification {
  def is = s2"""
    Can derive a Generic with explicit Out for a scrooge-generated struct $canDeriveGenExplicit
    Can derive a Generic  for a scrooge-generated struct $canDeriveGen
    Doesn't resolve for non-scrooge types               $nonScrooge
    Can derive Ordering                                 $deriveOrdering
    """

  def canDeriveGen = {
    import ThriftGen._
    val gen = implicitly[Generic[Demo]]

    gen.to(Demo("Hello", "World")) === "Hello" :: "World" :: None :: HNil
  }

    def canDeriveGenExplicit = {
      import ThriftGen._
      val genExplicit = implicitly[Generic.Aux[Demo, String :: String :: Option[Int] :: HNil]]

      val hlist : String :: String :: Option[Int] :: HNil = genExplicit.to(Demo("Hello", "World", Some(31)))
      hlist === "Hello" :: "World" :: Some(31) :: HNil
    }

    def nonScrooge = {
      case class A(a: Int)
      illTyped("""
        ThriftGen.thriftGen[A]
      """)
      1 === 1
    }

  def deriveOrdering = {
    //This is the problem I was initially solving

    // Code from https://github.com/milessabin/shapeless/blob/master/examples/src/main/scala/shapeless/examples/ordering.scala

    /// BEGIN

    // Derive an Ordering for an HList from the Orderings of its elements
    trait LowPriorityGenericOrdering {
      // An Ordering for any type which is isomorphic to an HList, if that HList has an Ordering

      implicit def hlistIsoOrdering[A, H <: HList](implicit gen: Generic.Aux[A, H], oh: Ordering[H]): Ordering[A] = new Ordering[A] {
        def compare(a1: A, a2: A) = oh.compare(gen to a1, gen to a2)
      }
    }

    object GenericOrdering extends LowPriorityGenericOrdering {
      implicit def hnilOrdering: Ordering[HNil] = new Ordering[HNil] {
        def compare(a: HNil, b: HNil) = 0
      }

      implicit def hlistOrdering[H, T <: HList](implicit oh: Ordering[H], ot: Ordering[T]): Ordering[H :: T] = new Ordering[H :: T] {
        def compare(a: H :: T, b: H :: T) = {
          val i = oh.compare(a.head, b.head)
          if (i == 0) ot.compare(a.tail, b.tail)
          else i
        }
      }
    }

    ///END

    import ThriftGen._
    import GenericOrdering._

    implicitly[Ordering[Demo]].compare(Demo("a", "b"), Demo("b", "c")) === -1
  }
}
