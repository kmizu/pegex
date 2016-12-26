package com
package github
package kmizu
package pegex

import Pegex._
import org.scalatest.FeatureSpec

class PegexBasicSpec extends FeatureSpec {
  feature("PEGEXs") {
    scenario("alphabet sequences") {
      val alphabets = """[a-zA-Z]*$;""".e
      assert(alphabets.matches("Hoge") === Some("Hoge"))
      assert(alphabets.matches("HogeFooBar") === Some("HogeFooBar"))
      assert(alphabets.matches("Hoge_Foo_Bar") === None)
    }

    scenario("identifiers") {
      val ident = """#(IdentStart)#(IdentRest)*$;
      IdentStart=[a-zA-Z_];
      IdentRest=#(IdentStart)|[0-9];
                  """.e
      List("HogeFooBar" -> Some("HogeFooBar"),
        "Hoge_Foo_Bar" -> Some("Hoge_Foo_Bar"),
        "Hoge10" -> Some("Hoge10"),
        "10Hoge" -> None
      ).foreach{ case (input, expectation) =>
        assert(ident.matches(input) === expectation)
      }
    }

    scenario("nested comments") {
      val comment = """#(C)$; C=/\*(#(C)|(?!\*/).)*\*/;""".e
      assert(comment.matches("/* comment */") === Some("/* comment */"))
      assert(comment.matches("/* nested /* comment */ ok */") === Some("/* nested /* comment */ ok */"))
      assert(comment.matches("/* incorrect comment") === None)
    }

    scenario("a context sensitive language") {
      val csl = """(?=#(A)(?!b))a+#(B)$; A=a#(A)?b; B=b#(B)?c;""".e
      assert(csl.matches("aaabbbccc") === Some("aaabbbccc"))
      assert(csl.matches("aabbbccc") === None)
    }

    scenario("palindrome")  {
      val palindrome = """#(A)$; A=a#(A)a|b#(A)b|c#(A)c|a|b|c|_;""".e
      assert(palindrome.matches("abcba") === Some("abcba"))
      assert(palindrome.matches("abba") === Some("abba"))
      assert(palindrome.matches("abc") === None)
    }

    scenario("name:value pair") {
      val nv = peg"""#(N::[a-zA-Z_][a-zA-Z0-9_]*):#(V::[0-9]+)$$;"""
      val r1 = nv.matchesWithGroup("a:1")
      assert(r1.result === Some("a:1"))
      assert(r1.group.get('N) === Some("a"))
      assert(r1.group.get('V) === Some("1"))
      assert(r1('N) === "a")
      assert(r1('V) === "1")
    }
  }

  feature("Pathologic PEGEX instances") {
    scenario("nested repetition should terminate successfully") {
      val hoge = peg"(a*)*;"
      assert(hoge.matches("aaaaaaa") === Some("aaaaaaa"))
    }
  }

  feature("PEGEX with back reference"){
    scenario("(ab)^n, where n is even number") {
      val testBackref = """#(A::(ab)+)##(A)$;""".e
      assert(testBackref.matches("ab") === None)
      assert(testBackref.matches("abab") === Some("abab"))
      assert(testBackref.matches("ababab") === None)
      assert(testBackref.matches("abababab") === Some("abababab"))
    }

    scenario("XMLs") {
      val minXml = """#(E)$; E=<#(tag:I)>#(E)*</##(tag)>; I=[a-z]+;""".e
      assert(minXml.matches("<foo></foo>") === Some("<foo></foo>"))
      assert(minXml.matches("<foo><bar></bar></foo>") === Some("<foo><bar></bar></foo>"))
      assert(minXml.matches("<foo><bar></bar></hoo>") === None)
    }

    scenario("hat(^) and dollar($)") {
      val hello = peg"^Hello$$;"
      assert(hello.matches("Hello") === Some("Hello"))
      assert(hello.matches("Hello, World") === None)
    }
  }

  feature("PEGEX with positive lookahead") {
    scenario("?=ab") {
      val abLA = peg"(?=ab)"
      assert(abLA.matches("ab") === Some(""))
      assert(abLA.matches("bc") === None)
      val ab = peg"ab"
      assert(ab.matches("ab") === Some("ab"))
      assert(ab.matches("bc") === None)
    }
  }

  feature("PEGEX with negative lookahead") {
    scenario("?!ab") {
      val abNLA = peg"(?!ab)"
      assert(abNLA.matches("ab") === None)
      assert(abNLA.matches("bc") === Some(""))
      val ab = peg"ab"
      assert(ab.matches("ab") === Some("ab"))
      assert(ab.matches("bc") === None)
    }
  }
}
