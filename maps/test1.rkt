#lang map

HEAD {
}

ROOM sr {
	PARASITE {
		.pos-x 100
		on-floor
	}
	DOOR {
		.place 'right
		.dest hallway
	}
}

ROOM hallway {
	.width 400

	DOOR {
		.place 'left
		.dest sr
	}
}

; vim: set ts=4 sw=4 noexpandtab tw=79:
