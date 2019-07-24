## PEGEX: A PEG-based pattern matching library EXtended by back reference with regex-like notation in Scala [![Build Status](https://travis-ci.org/kmizu/pegex.png?branch=master)](https://travis-ci.org/kmizu/pegex)

[![Join the chat at https://gitter.im/kmizu/pegex](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/kmizu/pegex?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/kmizu/pegex.png?branch=master)](https://travis-ci.org/kmizu/pegex)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.kmizu/pegex_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.kmizu/pegex_2.13)
[![Scaladoc](http://javadoc-badge.appspot.com/com.github.kmizu/pegex_2.13.svg?label=scaladoc)](http://javadoc-badge.appspot.com/com.github.kmizu/pegex_2.13/index.html#com.github.kmizu.pegex.package)
[![Reference Status](https://www.versioneye.com/java/com.github.kmizu:pegex_2.13/reference_badge.svg?style=flat)](https://www.versioneye.com/java/com.github.kmizu:pegex_2.13/references)

PEGEX (Parsing Expression Grammar EXtended by back reference with regex-like notations) is a PEG-like pattern matching  library for Scala.  
PEGEX provides both power of PEG and light-weight syntax of regular expressions.  It accepts a PEG-variant with regex-like notations.  
Currently, PEGEXes seem stronger than CFGs and alike [Boolean Grammars](https://en.wikipedia.org/wiki/Boolean_grammar).

## Usage

Add the following line to your `build.sbt`

```scala
libraryDependency += "com.github.kmizu" %% "pegex" % "1.0.0"
```

## Requirement
* Scala 2.11.X, Scala 2.12.X, and Scala 2.13.X
* JDK 1.8.0 or later

## Build Requirement
* sbt 1.2.8 or later

## Example
See [tests](https://github.com/kmizu/pegex/tree/master/src/test/scala/com/github/kmizu/pegex).

## Syntax of PEGEX
(*name*=*e*;)\*, where *name* is name of this rule and *e* is an expression

### Kind of Expressions (*e*)

**Note that in PEGEX, space characters have meanings. 
Thus, `A=;` is different from `A= ;`**

- `e1e2`: sequence of expressions
  - `"ab"`
  - `"ac"`
- `e1|e2`: unordered choice of expressions
  - `"a|b"`
  - `"(ab|bc)"`
- `e*`: zero or more repetition
  - `"a*"`
- `e+`: one or more repetition.
  - `"a+"`
- `e?`: zero-or-one repetition.
  - `"(a|b)?"`
- `(?=e)`: positive lookahead.
  - `"(?=a)"`
- `(?!e)`: negative lookahead
  - `"(?!a)"`
- `(?<name>e)`: named capture
-  `\g'E'`: (recursive) rule invocation of `E`
   - is identical to `#{E}`
- `\k<name>`: back reference of the named capture of `name`

```
#{E}$; E=<(?<tag>#{I})>#{E}*</\k<tag>>; I=[a-z]+;
```

- .: any character.
  - `"(?!a)."` matches one character except `a`
- _: success certainly.
- *x*: one character.
- `[f-t...xyz...]`: character class.  e.g. `[a-zA-Z_]`

## Releate Note

### 1.0.0

- Support Scala 2.11.X, 2.12.X, and 2.13.X
- From this version, follow Semantic Versioning 
