#lang brag
program ::= clause*

@clause ::= head-clause | room-clause

head-clause ::= "HEAD" /":" clause-body
room-clause ::= "ROOM" id /":" clause-body
@id ::= word

clause-body ::= clause-body-line*
@clause-body-line ::= assignment
                    | directive
                    | entity-clause

assignment ::= member-id rvalue
member-id ::= /"." word
rvalue ::= str
         | word
         | symbol
         | number

directive ::= word

entity-clause ::= clause-name [id] /":"
@clause-name ::= ucase-word

str ::= STR-TOK
ucase-word ::= UCASE-WORD-TOK
word ::= WORD-TOK
symbol ::= /"'" word
number ::= int | float
int ::= INT-TOK
float ::= int+ "." int+

; vim: set ts=2 sw=2 expandtab lisp tw=79:
