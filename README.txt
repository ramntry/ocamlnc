Binary Annotations
==================

Author: Fabrice LE FESSANT (INRIA/OCamlPro)
Date: 2010/11/03 - 2010/11/13

The goal of this branch is to replace textual .annot files by binary .types
files, containing the Typedtree of the compiled file. If compilation
succeeded, the file contains a [| Saved_implementation str |], and otherwise,
it contains an array of all the typed nodes that were correctly typed.

- Typedtree has been extended to become a superset of Parsetree
- It is possible to revert from Typedtree to Parsetree
- It is possible to revert from Parsetree to sources

===========================================================================

In this version, when -annot is provided while compiling XXXX.ml:
 * A XXXX.annot file is generated for backward compatibility
 * A XXXX.types file is generated, containing a dump of a
   typedtree.structure
 * A XXXX_ast2src.ml is generated, containing the sources obtained
   from the parsetree by pretty-printing.
 * A XXX_typ2src.ml is generated, containing the sources obtained
   by generating a parsetree from the typedtree and pretty-printing
   it.

module Typedtree is extended with the following functions:

val untype_structure : structure -> Parsetree.structure
val untype_signature : signature -> Parsetree.signature
val print_structure : Format.formatter -> Parsetree.structure -> unit

===========================================================================

Contributions:
  The first version of the parsetree pretty-printer was contributed
by BER-metaocaml and improved.
