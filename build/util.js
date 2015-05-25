var fs = require('fs');
var PNG = require('node-png').PNG;
var Canvas = require('canvas');

function min(x, y) {
  return x < y ? x : y;
}

function max(x, y) {
  return x > y ? x : y;
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

function with_image(filename, k) {
  fs.createReadStream(filename)
    .pipe(new PNG({ filterType: 4 }))
    .on('parsed', function () {
      var imdat = this;
      c = new Canvas(imdat.width,imdat.height);
      d = c.getContext('2d');
      k(imdat, d);
    });
}

function output_image(filename, d, imdat) {
  d.putImageData(imdat, 0, 0);
  c.pngStream().pipe(fs.createWriteStream(filename));
}

exports.min = min;
exports.max = max;
exports.get = get;
exports.get3 = get3;
exports.set3 = set3;
exports.with_image = with_image;
exports.output_image = output_image;
