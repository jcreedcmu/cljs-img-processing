var _ = require('underscore');
var extents = require('./extents');
var u = require('./util');
var fs = require('fs');

int = Math.floor;

function get_sizes_from_extents(extents) {
  return _.object(
    _(extents)
      .map(function(v, k) {
	var size = {x: v.max_x - v.min_x + 2,
		    y: v.max_y - v.min_y + 2};
	var area = size.x * size.y;
	if (area > 100000) return null;
	return [k, size];
      })
      .filter(_.identity)
  );
}

extents.get_extents(function(extents, imdat, c) {

  var sizes = get_sizes_from_extents(extents);
  var colors = _.keys(sizes);

  var biggest = _(["x", "y"]).map(
    function(coord) {
      return [coord, _.max(_(sizes).map(function(v, k) { return v[coord] }))];
    });
  biggest = _.object(biggest);

  var num_cells = Math.ceil(Math.sqrt(_.keys(sizes).length));

  var w = num_cells * biggest.x;
  var h = num_cells * biggest.y;

  var cc = u.buffer(w, h);
  var newdat = cc.d.createImageData(w, h);
  var clear = [0,0,0,0];
  var black = [0,0,0,255];
  for (var y = 0; y < newdat.height; y++) {
    var ybase = int(y / biggest.y)
    for (var x = 0; x < newdat.width; x++) {
      var xbase = int(x / biggest.x);
      var index = ybase * num_cells + xbase;
      var color = colors[index];

      var pix = false;
      if (color != null)  {
	if (extents[color] == null) { console.log('extents undefined'); console.log(color); console.log(extents); }
	var orig_x = x - biggest.x * xbase + extents[color].min_x;
	var orig_y = y - biggest.y * ybase + extents[color].min_y;
	pix = true;
	pix = (color == u.get_slash(imdat, orig_x, orig_y));
      }
      u.set4(newdat, x, y, pix ? black : clear);
    }
  }

  u.output_image(__dirname + '/../built/map-pieces.png', cc, newdat);
  fs.writeFileSync(__dirname + '/../built/map-pieces.json', JSON.stringify(
    {colors:colors,
     num_cells:num_cells,
     cell_size:biggest,
     sizes:sizes,
     extents:extents,
     orig_image_size:{width:imdat.width,height:imdat.height}}), 'utf8');
});
