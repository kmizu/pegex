# PEGEX: A PEG-based pattern matching library EXtended by back reference with regex-like notation in Scala

PEGEX (Parsing Expression Grammar EXtended by back reference with regex-like notations) is 
a PEG-based pattern matching  library for Scala.  PEGEX provides both power of PEG and 
light-weight syntax of regular expressions.  It accepts a PEG-variant with regex-like 
notations as inputs and parses strings.

## Runtime Requirement
+ scala 2.8.0 or later
+ jdk 1.5.0 or later

## Build Requirement
+ scala 2.8.0 or later
+ jdk 1.5.0 or later
+ Apache Ant 1.6 or later

## Build Instructions
1. Move to project's root directory.
   > cd pegex
2. Type "ant" in shell.
   > ant
3. Deploy the file "pegex.jar", which is library jar file generated from source files.

## Syntax of PEGEX
(*name*=*e*;)*, where *name* is name of this rule and *e* is an expression

### Kind of Expressions (*e*)
+ e1e2: sequence.  e.g. ab
+ e1|e2: ordered chocie.  e.g. a|b
+ e*: repetition (>= 0).  e.g. a*
+ e+: repetition (>= 1).  e.g. a+
+ e?: zero-or-one occurrence.  e.g? e.g. a?
+ &e: and predicate.  e.g. &a
+ !e:  not predicate. e.g. !a
+ _: any character.
+ *x*: one character.  e.g. a
+ [f-t...xyz...]: character class.  e.g. [a-zA-Z_]
+ #(*name*):  reference of other rule.  e.g. #(Ident)
  + An expression #(*name*:*e*) binds parsing result of e to name
+ backreference: ##(*name*) e.g. #(I:Ident):##(I)