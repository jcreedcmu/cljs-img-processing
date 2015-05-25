var _ = require('underscore');
var u = require('./util');

u.with_image(__dirname + '/../resources/public/map.png', function(imdat, d) {
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


  console.log(extent);
});

function optimizePts(pts) {
  var newpts = [];
  for (var i = 0; i < pts.length; i++) {
   if (i % 3 == 0 || i == pts.length - 1)
     newpts.push(pts[i]);
  }
  return newpts;
}
