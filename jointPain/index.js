var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);

app.get('/', function(req, res){
    res.sendFile(__dirname + '/index.html');
});

io.on('connection',function(socket){
    console.log('a user connected');
    // emits to everyone except active socket
    socket.broadcast.emit('hi');
    socket.on('chat message', function(msg){
        // emit message to everyone
        io.emit('chat message', msg);
        console.log('message: ' + msg);
    });
    socket.on('disconnect', function(){
        console.log('user disconnected');
    });
});

http.listen(3000, function(){
    console.log('listening on localhost:3000');
});
