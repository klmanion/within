#lang map

HEAD:

ROOM sr:
	PARASITE:
		.pos-x 100
		.pos-y 100

	DOOR:
		.pos 'right
		.dest hallway

ROOM hallway:
	DOOR:
		.pos 'left
		.dest sr

; vim: set ts=4 sw=4 noexpandtab tw=79:
