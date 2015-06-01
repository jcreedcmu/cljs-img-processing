var _ = require('underscore');
var u = require('./util');

exports.get_extents = function (k) {
  u.with_image(__dirname + '/../resources/public/map.png', function(imdat, c) {
    var extent = {};
    for (var y = 0; y < imdat.height; y++) {
      for (var x = 0; x < imdat.width; x++) {
	var here = u.get3(imdat, x, y).join("/");
	if (!extent[here])
	  extent[here] = {min_x: 1e9, max_x: 0,
			  min_y: 1e9, max_y: 0};
	var e = extent[here];
	e.min_x = u.min(e.min_x, x);
	e.min_y = u.min(e.min_y, y);
	e.max_x = u.max(e.max_x, x);
	e.max_y = u.max(e.max_y, y);
      }
    }
    k(extent, imdat, c);
  });
}
