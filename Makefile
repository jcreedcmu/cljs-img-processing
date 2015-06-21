all: built/map-outline.png \
     built/map-pieces.png \
     built/map-pieces.json

clean:
	rm -rf built/*
	touch build/.gitkeep

# This sneaky trick gotten from
# http://stackoverflow.com/questions/2973445/gnu-makefile-rule-generating
built/map-%ieces.png build/map-%ieces.json:
	cd build; node reimage.js

built/map-outline.png:
	cd build; node map.js
