package uk.co.turingatemyhamster.shortbol

import utest._

object PrefixBindingsTestSuite extends TestSuite {

  val tests = TestSuite {
    val anon = Url("http://example.com/")
    val pfx = NSPrefix("foo")
    val ln = LocalName("bar")
    val ns = Url("http://foo.com/")

    'empty - {
      val pb = PrefixBindingsImpl(None, Map.empty)

      * - assert(pb.resolve(None) == None)
      * - assert(pb.resolve(Some(pfx)) == None)
      * - assert(pb.resolve(Some(pfx), ln) == None)
      * - assert(pb.resolve(None, ln) == None)
    }

    'withAnon - {
      val pb = PrefixBindingsImpl(Some(anon), Map.empty)

      * - assert(pb.resolve(None) == Some(anon))
      * - assert(pb.resolve(Some(pfx)) == None)
      * - assert(pb.resolve(Some(pfx), ln) == None)
      * - assert(pb.resolve(None, ln) == Some(Url(anon.url + ln.name)))
    }

    'withBinding - {
      val pb = PrefixBindingsImpl(None, Map(pfx -> ns))

      * - assert(pb.resolve(None) == None)
      * - assert(pb.resolve(Some(pfx)) == Some(ns))
      * - assert(pb.resolve(Some(pfx), ln) == Some(Url(ns.url + ln.name)))
      * - assert(pb.resolve(None, ln) == None)
    }
  }

}
