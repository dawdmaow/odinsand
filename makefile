build:
	odin build . -debug -sanitize:address -o:none

run:
	odin run . -debug -sanitize:address -o:none