# PEGEX: A PEG-based pattern matching library EXtended by back reference with regex-like notation in Scala [![Build Status](https://travis-ci.org/kmizu/pegex.png?branch=master)](https://travis-ci.org/kmizu/pegex)

[![Join the chat at https://gitter.im/kmizu/pegex](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/kmizu/pegex?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

PEGEX (Parsing Expression Grammar EXtended by back reference with regex-like notations) is 
a PEG-based pattern matching  library for Scala.  PEGEX provides both power of PEG and 
light-weight syntax of regular expressions.  It accepts a PEG-variant with regex-like 
notations as inputs and parses strings.

## For sbt user

Include the following in your `build.sbt` to add `pegex` to your project libraryDependencies:

```scala
resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependency += "com.github.kmizu" %% "pegex" % "0.3.0-SNAPSHOT"
```

## Runtime Requirement
* Scala 2.9.1, Scala 2.9.2, Scala 2.9.3, Scala 2.10.X, Scala 2.11.X
* JDK 1.6.0 or later

## Build Requirement
* sbt 0.13.X

## Build Instructions
1. Move to project's root directory.

   `$ cd pegex`
   
2. Type "sbt" in shell.

   `$ sbt`

3. In the sbt shell, type as the following to package the files to `pegex_${scalaVersion}-${version}-SNAPSHOT.jar`

   `> package`
   
## Example of PEGEX
See `*.pegex` files under `example` directory.  These are valid PEGEXes.

## Syntax of PEGEX
(*name*=*e*;)\*, where *name* is name of this rule and *e* is an expression

### Kind of Expressions (*e*)
**Note that in PEGEX, aspace has meaning. Thus, `A=;` is different from `A= ;`**
+ e1e2: sequence.  e.g. ab
+ e1|e2: ordered chocie.  e.g. a|b
+ e*: repetition (>= 0).  e.g. a*
+ e+: repetition (>= 1).  e.g. a+
+ e?: zero-or-one occurrence.  e.g? e.g. a?
+ &e: and predicate.  e.g. &a
+ !e:  not predicate. e.g. !a
+ .: any character.
+ _: success certainly.
+ *x*: one character.  e.g. a
+ [f-t...xyz...]: character class.  e.g. [a-zA-Z_]
+ \#(*name*):  reference of other rule.  e.g. #(Ident)
  + An expression #(*name*:*e*) binds parsing result of e to name
+ ##(*name*):backreference e.g. #(I:Ident):##(I)
