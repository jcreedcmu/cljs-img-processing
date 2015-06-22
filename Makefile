all: built/map-outline.png \
     built/map-pieces.png \
     built/map-pieces.json \
     built/adjacencies.json

clean:
	rm -rf built/*
	touch built/.gitkeep

# This sneaky trick gotten from
# http://stackoverflow.com/questions/2973445/gnu-makefile-rule-generating
built/map-%ieces.png build/map-%ieces.json: resources/public/map.png
	node build/reimage.js

built/map-outline.png: resources/public/map.png
	node build/map.js

built/adjacencies.json: resources/public/map.png
	node build/adjacencies.js
