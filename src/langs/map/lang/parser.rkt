#lang brag
program ::= head-clause clause*

head-clause ::= clause

clause ::= clause-head clause-body

clause-head ::= clause-name [id] /":"
clause-name ::= ucase-word
id ::= word

clause-body ::= clause-body-line*
@clause-body-line ::= (assignment | directive | contained)

assignment ::= member-id rvalue
member-id ::= /"." word
rvalue ::= str
         | word
         | int

directive ::= word

contained ::= clause

str ::= STR-TOK
ucase-word ::= UCASE-WORD-TOK
word ::= WORD-TOK
int ::= INT-TOK

; vim: set ts=2 sw=2 expandtab lisp tw=79:
