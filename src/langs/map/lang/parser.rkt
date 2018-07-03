#lang brag
program ::= head-clause room-clause*

head-clause ::= head-clause-head head-clause-body
head-clause-head ::= "HEAD" ":"
head-clause-body ::= clause-body

room-clause ::= room-clause-head room-clause-body
room-clause-head ::= "ROOM" id ":"
id ::= word
room-clause-body ::= clause-body

clause-body ::= (assignment | directive | contained)*

assignment ::= member-id rvalue
member-id ::= "." word
rvalue ::= str
         | int

directive ::= word

contained ::= clause
clause ::= clause-head clause-body
clause-head ::= clause-name ":"
clause-name ::= ucase-word

str ::= word*
ucase-word ::= UCASE-WORD-TOK
word ::= WORD-TOK
int ::= INT-TOK

; vim: set ts=2 sw=2 expandtab lisp tw=79:
