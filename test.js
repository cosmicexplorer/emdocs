var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);
var client_io = require('socket.io-client');

const global_http_port = 8080;

http.listen(global_http_port, function() {
  console.log("listening on port " + global_http_port);
})

io.on('connection', function(socket) {
  console.log("client connected");
  socket.on('BEES!', function(dataStr) {
    console.log("BEES!: " + dataStr);
    socket.emit('BEES_ACKNOWLEDGED', "BEES_ACK_STR");
  });
});

init(client_io("http://127.0.0.1:8080"));
init(client_io("http://172.31.28.0:8080"));

function init(socket) {
  socket.on('connect', function() {
    console.log("server connected");
    socket.emit('BEES!', "BEES_STRING");
    socket.on('BEES_ACKNOWLEDGED', function(dataStr) {
      console.log("BEES_ACKNOWLEDGED: " + dataStr)
    });
  });
}
