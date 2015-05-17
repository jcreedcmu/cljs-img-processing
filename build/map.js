var fs = require('fs'),
    PNG = require('node-png').PNG;
var _ = require('underscore');

function parse_int(x) {
  return parseInt(x);
}

function get(im, x, y) {
  var g = get3(im, x, y);
  return g[1];
}

function get3(im, x, y) {
   if (x < 0 || x >= im.width || y < 0 || y >= im.height) return [255,255,255];

  var ix = (im.width * y + x) << 2;
  return [im.data[ix],
	  im.data[ix + 1],
	  im.data[ix + 2]];
}


function set3(im, x, y, c) {
  var base = (im.width * y + x) << 2;
  im.data[base] = c[0];
  im.data[base + 1] = c[1];
  im.data[base + 2] = c[2];
  im.data[base + 3] = 255;
}

function go() {


  var im = this;

  var Canvas = require('canvas')
  c = new Canvas(im.width,im.height);
  d = c.getContext('2d');
  d.fillStyle = "#def";
  var newd = d.createImageData(im.width, im.height);

  for (var y = 0; y < this.height; y++) {
    for (var x = 0; x < this.width; x++) {
      var here = get(im, x, y);
      var others = [get(im, x-1, y), get(im, x, y-1), get(im, x, y+1), get(im, x+1, y)]

      set3(newd, x, y, (here > others[0] || here > others[1] || here > others[2] || here > others[3])  ?
	   [0,0,0] :
	   (get(im, x, y) == 0x4b ?
	    [128, 192, 192] :
	    [255, 255, 255])  );
    }
  }


  d.putImageData(newd, 0, 0);

  c.pngStream().pipe(fs.createWriteStream(__dirname + '/map-out.png'));

  //  out.pack().pipe(fs.createWriteStream('out.png'));
}

fs.createReadStream('../resources/public/map.png')
    .pipe(new PNG({ filterType: 4 }))
    .on('parsed', go);

function optimizePts(pts) {
  var newpts = [];
  for (var i = 0; i < pts.length; i++) {
   if (i % 3 == 0 || i == pts.length - 1)
     newpts.push(pts[i]);
  }
  return newpts;
}
