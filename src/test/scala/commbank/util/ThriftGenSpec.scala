package commbank.util

import shapeless._, test.illTyped
import org.specs2._

import thrift.Demo

object ThriftGenSpec extends Specification {
  def is = s2"""
    Can derive a Generic for a scrooge-generated struct $canDeriveGen
    Doesn't resolve for non-scrooge types               $nonScrooge
    """

    def canDeriveGen = {
      import ThriftGen._
      val gen = implicitly[Generic.Aux[Demo, String :: String :: HNil]]

      val hlist : String :: String :: HNil = gen.to(Demo("Hello", "World"))
      hlist === "Hello" :: "World" :: HNil
    }

    def nonScrooge = {
      case class A(a: Int)
      illTyped("""
        ThriftGen.thriftGen[A]
      """)
      1 === 1
    }
}
