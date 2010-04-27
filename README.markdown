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
(name=e;)*, where name is name of this rule and e is an expression

### Kind of Expressions (e)
+ sequence: e1e2 e.g. ab
+ (ordered) chioce: e1|e2 e.g. a|b
+ repetition (>= 0): e* e.g. a*
+ repetition (>= 1): e+ e.g. a+
+ option: e? e.g. a?
+ and predicate: &e e.g. &a
+ not predicate: !e e.e. !a
+ any character: _
+ one character: x e.g. a
+ character class: [...] e.g. [a-zA-Z_]
+ reference of other rule: #(name) e.g. #(Ident)
  + #(name:e) binds parsing result of e to name
+ backreference: ##(name) e.g. #(I:Ident):##(I)