var _ = require('underscore');
var u = require('./util');

u.with_image(__dirname + '/../resources/public/map.png', function(imdat, c) {
  var sees = {};
  function add_edge(from, to) {
    if (!sees[from]) sees[from] = {};
    sees[from][to] = 1;
  }
  for (var y = 0; y < imdat.height; y++) {
    for (var x = 0; x < imdat.width; x++) {
      var here = u.get_slash(imdat, x, y);
      var up = u.get_slash(imdat, x, y-1);
      var left = u.get_slash(imdat, x-1, y);
      if (here != up) {
	add_edge(here, up);
	add_edge(up, here);
      }
      if (here != left) {
	add_edge(here, left);
	add_edge(left, here);
      }
    }
  }
  console.log(sees);
});
