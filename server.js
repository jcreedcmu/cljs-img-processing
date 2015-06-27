var exp = require('express');
var app = exp();
app.use('/', exp.static(__dirname + '/dist'));
app.listen(8000);
