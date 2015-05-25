var fs = require('fs'),
    PNG = require('node-png').PNG;
var _ = require('underscore');
var u = require('./util');

function go() {


  var im = this;

  var Canvas = require('canvas')
  c = new Canvas(im.width,im.height);
  d = c.getContext('2d');
  d.fillStyle = "#def";
  var newd = d.createImageData(im.width, im.height);

  for (var y = 0; y < this.height; y++) {
    for (var x = 0; x < this.width; x++) {
      var here = u.get(im, x, y);
      var others = [u.get(im, x-1, y), u.get(im, x, y-1), u.get(im, x, y+1), u.get(im, x+1, y)]

      u.set3(newd, x, y, (here > others[0] || here > others[1] || here > others[2] || here > others[3])  ?
	   [0,0,0] :
	   (u.get(im, x, y) == 0x4b ?
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
