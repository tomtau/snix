package snix.nix

import org.scalatest.FlatSpec
import snix.nix.Types._
import snix.nix.Parser._

class ParserTests extends FlatSpec {

  it should "parse int" in {
    assertParseString("234", mkInt(234))
  }

  it should "parse bool" in {
    assertParseString("true", mkBool(true))
    assertParseString("false", mkBool(false))
  }

  it should "parse constant paths" in {
    assertParseString("./.", mkPath(false, "./."))
    assertParseString("./+-_/cdef/09ad+-", mkPath(false, "./+-_/cdef/09ad+-"))
    assertParseString("/abc", mkPath(false, "/abc"))
    assertParseString("../abc", mkPath(false, "../abc"))
    assertParseString("<abc>", mkPath(true, "abc"))
    assertParseString("<../cdef>", mkPath(true, "../cdef"))
    assertParseString("a//b", mkOper2(NUpdate, (mkSym("a")), (mkSym("b"))))
    assertParseString("rec+def/cdef", mkPath(false, "rec+def/cdef"))
    assertParseString("a/b//c/def//<g> < def/d", mkOper2(NLt,
      (mkOper2(NUpdate, mkPath(false,"a/b"), mkOper2(NUpdate,
        mkPath(false, "c/def"), mkPath(true,"g")))), mkPath(false, "def/d")))
    assertParseString("a'b/c", Mu[NExprF](NApp(mkSym("a'b"), (mkPath(false,"/c")))))
    assertParseFail(".")
    assertParseFail("..")
    assertParseFail("/")
    assertParseFail("a/")
    assertParseFail("a/def/")
  }

  it should "parse constant uris" in {
    assertParseString("a:a", mkUri("a:a"))
    assertParseString("http://foo.bar", mkUri("http://foo.bar"))
    assertParseString("a+de+.adA+-:%%%ads%5asdk&/", mkUri("a+de+.adA+-:%%%ads%5asdk&/"))
    assertParseString("rec+def:c", mkUri("rec+def:c"))
    assertParseString("f.foo:bar", mkUri("f.foo:bar"))
    assertParseFail("http://foo${\"bar\"}")
    assertParseFail(":bcdef")
    assertParseFail("a%20:asda")
  }

  //TODO: port tests from https://github.com/jwiegley/hnix/blob/master/tests/ParserTests.hs


  def assertParseString(str: String, expected: NExpr) =  parseNixString(str) match {
    case Success(actual, _) => assert(actual === expected)
    case Failure(err, _) => fail("Unexpected error parsing `" + str + "':\n" + err)
  }

  def assertParseFail(str: String) = parseNixString(str) match {
    case Success(r, _) => fail( "Unexpected success parsing `" + str + ":\nParsed value: " + r.toString)
    case Failure(_, _) => assert(true)
  }
}
