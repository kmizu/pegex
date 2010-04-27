package jp.gr.java_conf.mizu.pegex
import org.specs._

object PegexBasicSpec extends Specification {
  import jp.gr.java_conf.mizu.pegex.Pegex._
  """PEGEX representing alphabet sequences""" in {
    val alphabets = """L=[a-zA-Z]+$;""".e
    alphabets.matches("HogeFooBar") must_== Some("HogeFooBar")
    alphabets.matches("Hoge_Foo_Bar") must_== None
  }
  """PEGEX representing identifiers""" in {
    val ident = """
      L=#(IdentStart)#(IdentRest)*$;
      IdentStart=[a-zA-Z_];
      IdentRest=#(IdentStart)|[0-9];
    """.e    
    ident.matches("HogeFooBar") must_== Some("HogeFooBar")
    ident.matches("Hoge_Foo_Bar") must_== Some("Hoge_Foo_Bar")
    ident.matches("Hoge10") must_== Some("Hoge10")
    ident.matches("10Hoge") must_== None
  }
  """PEGEX not matching anything""" in {
    val nothing = """L=a*a;""".e(likeRegex = false)
    nothing.matches("aaa") must_== None
  }
  """PEGEX(greedy mode) representing one or more repetition of "a"""" in {
    val a1OrMore = """L=a*a;""".e(likeRegex = true)
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
  """PEGEX representing (ab)^n, where n is even number.  This is test of backreference feature""" in {
    val testBackref = """L=#(A::(ab)+)##(A)$;""".e
    testBackref.matches("ab") must_== None
    testBackref.matches("abab") must_== Some("abab")
    testBackref.matches("ababab") must_== None
    testBackref.matches("abababab") must_== Some("abababab")
  }
  """PEGEX representing name:value pair""" in {
    val nv = """L=#(N::[a-zA-Z_][a-zA-Z0-9_]*)\:#(V::[0-9]+)$;""".e
    val r1 = nv.matchesWithGroup("a:1")
    r1.result must_== Some("a:1")
    r1.group.get('N) must_== Some("a")
    r1.group.get('V) must_== Some("1")
  }
  override def main(args: Array[String]) {
    super.main(args)
  }
}
