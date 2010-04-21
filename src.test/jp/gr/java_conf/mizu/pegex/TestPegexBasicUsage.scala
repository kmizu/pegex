package jp.gr.java_conf.mizu.pegex

object TestPegexBasicUsage {
  def main(args: Array[String]) {
    import jp.gr.java_conf.mizu.pegex.Pegex._
    val alphabets = """L=[a-zA-Z]+$;""".e
    println("parsing alphabet ...")
    println(alphabets.matches("HogeFooBar"))
    println(alphabets.matches("Hoge_Foo_Bar"))
    println("done.")
    val ident = """
      L=#(IdentStart)#(IdentRest)*$;
      IdentStart=[a-zA-Z_];
      IdentRest=#(IdentStart)|[0-9];
    """.e
    println("parsing identifier ...")
    println(ident.matches("HogeFooBar"))
    println(ident.matches("Hoge_Foo_Bar"))
    println(ident.matches("Hoge10"))
    println(ident.matches("10Hoge"))
    println("done.")
    val nothing = """L=a*a;""".e(likeRegex = false)
    println("parsing nothing ...")
    println(nothing.matches("aaa")) // It should be None
    println("done.")
    println("parsing a1OrMore ...")
    val a1OrMore = """L=a*a;""".e(likeRegex = true)
    println(a1OrMore.matches("aaa")) // It should be Some(aaa)
    println("done.")
    println("parsing nested comment ...")
    val comment = """L=#(C)$; C=/\*(#(C)|!(\*/).)*\*/;""".e
    println(comment.matches("/* comment */"))
    println(comment.matches("/* nested /* comment */ ok */"))
    println(comment.matches("/* incorrect comment"))
    println("done.")
    println("parsing minXml ...")
    val minXml = """
      L=#(E)$; E=<#(tag:I)>#(E)*</##(tag)>; I=[a-z]+;""".e
    println(minXml.matches("<foo></foo>"))
    println(minXml.matches("<foo><bar></bar></foo>"))
    println(minXml.matches("<foo><bar></bar></hoo>"))
    println("done.")
    println("parsing csl ...")
    val csl = """S=&(#(A)!b)a+#(B)$; A=a#(A)?b; B=b#(B)?c;""".e
    println(csl.matches("aaabbbccc"))
    println(csl.matches("aabbbccc"))
    println("done.")
    println("parsing palindrome ...")
    val palindrome = new Pegex("""
      L=#(A)$; A=a#(A)a|b#(A)b|c#(A)c|a|b|c|_;
    """, likeRegex = true)
    println(palindrome.matches("abcba"))
    println(palindrome.matches("abba"))
    println(palindrome.matches("abc"))
    println("done.")
    println("parsing testBackref ...")
    val testBackref = new Pegex("""
      L=#(A::(ab)+)##(A)$;
    """, likeRegex = true)
    println(testBackref.matches("ab"))
    println(testBackref.matches("abab"))
    println(testBackref.matches("ababab"))
    println(testBackref.matches("abababab"))
  }
}
