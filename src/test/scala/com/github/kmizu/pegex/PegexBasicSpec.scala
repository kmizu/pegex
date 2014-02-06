package com
package github
package kmizu
package pegex

import Pegex._
import org.specs2.mutable.Specification

object PegexBasicSpec extends Specification {
  """PEGEX representing alphabet sequences""" in {
    val alphabets = """L=[a-zA-Z]+$;""".e
    alphabets.matches("Hoge") must_== Some("Hoge")
    alphabets.matches("HogeFooBar") must_== Some("HogeFooBar")
    alphabets.matches("Hoge_Foo_Bar") must_== None
  }

    val ident = """
      L=#(IdentStart)#(IdentRest)*$;
      IdentStart=[a-zA-Z_];
      IdentRest=#(IdentStart)|[0-9];
    """.e    
    List("HogeFooBar" -> Some("HogeFooBar"),
         "Hoge_Foo_Bar" -> Some("Hoge_Foo_Bar"),
         "Hoge10" -> Some("Hoge10"),
         "10Hoge" -> None
    ).foreach{ case (input, expectation) =>
      """PEGEX representing identifiers""" in {
        ident.matches(input) must_== expectation
      }
    }
  
  """PEGEX that greedy interpreter can parse successfully and possessive interpreter fails""" in {
    val ex = """L=a*a;"""
    val nothing = ex.e(likeRegex = false)
    nothing.matches("aaa") must_== None
    val a1OrMore = ex.e
    a1OrMore.matches("aaa") must_== Some("aaa")
  }

  """PEGEX representing nested comments""" in {
    val comment = """L=#(C)$; C=/\*(#(C)|!(\*/).)*\*/;""".e
    comment.matches("/* comment */") must_== Some("/* comment */")
    comment.matches("/* nested /* comment */ ok */") must_== Some("/* nested /* comment */ ok */")
    comment.matches("/* incorrect comment") must_== None
  }
  """PEGEX representing subset of XMLs""" in {
    val minXml = """
      L=#(E)$; E=<#(tag:I)>#(E)*</##(tag)>; I=[a-z]+;""".e
    minXml.matches("<foo></foo>") must_== Some("<foo></foo>")
    minXml.matches("<foo><bar></bar></foo>") must_== Some("<foo><bar></bar></foo>")
    minXml.matches("<foo><bar></bar></hoo>") must_== None
  }
  """PEGEX representing a context sensitive language""" in {
    val csl = """S=&(#(A)!b)a+#(B)$; A=a#(A)?b; B=b#(B)?c;""".e
    csl.matches("aaabbbccc") must_== Some("aaabbbccc")
    csl.matches("aabbbccc") must_== None
  }
  """PEGEX representing palindromes""" in {
    val palindrome = """L=#(A)$; A=a#(A)a|b#(A)b|c#(A)c|a|b|c|_;""".e(likeRegex=true)
    palindrome.matches("abcba") must_== Some("abcba")
    palindrome.matches("abba") must_== Some("abba")
    palindrome.matches("abc") must_== None
  }
  """PEGEX representing palindrome (incorrect)""" in {
    val palindrome = """L=#(A)$; A=a#(A)a|b#(A)b|c#(A)c|a|b|c|_;""".e(likeRegex=false)
    palindrome.matches("a" * 3) must_== Some("a" * 3)
    palindrome.matches("a" * 7) must_== Some("a" * 7)
    palindrome.matches("a" * 15) must_== Some("a" * 15)
    palindrome.matches("a" * 31) must_== Some("a" * 31)
    palindrome.matches("a" * 63) must_== Some("a" * 63)
  }
  """PEGEX representing (ab)^n, where n is even number.  This is test of backreference feature""" in {
    val testBackref = """L=#(A::(ab)+)##(A)$;""".e
    testBackref.matches("ab") must_== None
    testBackref.matches("abab") must_== Some("abab")
    testBackref.matches("ababab") must_== None
    testBackref.matches("abababab") must_== Some("abababab")
  }
  """PEGEX representing name:value pair""" in {    
    val nvStr = """L=#(N::[a-zA-Z_][a-zA-Z0-9_]*)\:#(V::[0-9]+)$;"""
    val nvGreedy = nvStr.e
    val r1 = nvGreedy.matchesWithGroup("a:1")
    r1.result must_== Some("a:1")
    r1.group.get('N) must_== Some("a")
    r1.group.get('V) must_== Some("1")
    r1('N) must_== "a"
    r1('V) must_== "1"
    val nvPossessive = nvStr.e(likeRegex = false)
    val r2 = nvPossessive.matchesWithGroup("a:1")
    r2.result must_== Some("a:1")
    r2.group.get('N) must_== Some("a")
    r2.group.get('V) must_== Some("1")    
  }
}
