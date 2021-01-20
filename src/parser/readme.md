# Parsing
A textual data is made up of different characters, spaces and black lines are considered characters as well.
The textual data has to be parsed for `odoc` to make sense out of it; The process of parsing a comment/textual data
is split into two phases, _lexing_ and _parsing_.

**Note**: It’s confusing that the term parsing is applied to both the overall process of converting textual data to structured data,
and also more specifically to the second phase of converting a stream of tokens to an AST.

## Lexing phase
In odoc, we use `ocamllex` to analyze the comment by including `ocamllex` stanza in a dune file, that is to say
`(ocamllex <name>)` where name is the input file with a `.mll` extension to ocamllex. For our case, it's lexer, see `lexer.mll`.

The lexer then generates a token for each comment character which are of type `Token.t}` defined in module `Token`.

## Parsing phase
Odoc defines it's own parser and the syntax for handling tokens in module `Syntax`. module `Odoc_parser` is the entry point
for the lexer and parser (in `make_parser` function).

The semantics by which the parser consumes the tokens to produce richer tree-like data structure called abstract syntax tree (module `Ast`)
by the lexing phase in handled in module `Semantics`.

The module `Parse_error` defines error messages for error that occur during parsing.

Module `Reference.parse` is a whole new parser because it defines own rules for textual data parsing.


