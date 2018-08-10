#lang brag
program ::= clause*

clause ::= clause-head clause-body

clause-head ::= clause-name [id]
clause-name ::= ucase-word
@id ::= word

clause-body ::= /"{" clause-body-line* /"}"
@clause-body-line ::= assignment
                    | directive
                    | clause

assignment ::= lvalue rvalue
lvalue ::= /"." word
rvalue ::= str-lit
         | word
         | symbol
         | number

directive ::= word

@str-lit ::= STR-LIT-TOK
@ucase-word ::= UCASE-WORD-TOK
@word ::= WORD-TOK
symbol ::= /"'" word
@number ::= int | float
@int ::= INT-TOK
@float ::= int+ "." int+

; vim: set ts=2 sw=2 expandtab lisp tw=79:
