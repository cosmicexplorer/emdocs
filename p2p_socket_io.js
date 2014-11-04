var app = require('express')();
var http = require('http').Server(app);
var hash_table = require('hashtable');
var dns = require('dns');
var os = require('os');
var io = require('socket.io')(http);
var client_io = require('socket.io-client');


function p2p_client(initialServerUri, http_port) {
  this.otherServerUri = initialServerUri;
  this.http_port = http_port;
  this.selfLocalIpAddr = "127.0.0.1";
  this.selfLocalUri = "http://" + this.selfLocalIpAddr + ':' + http_port;
  this.socketTable = new hash_table();
}

p2p_client.getUriOfSocket = function(socket) {
  return "http://" + socket.io.opts.host + ':' + socket.io.opts.port;
}

// do everything with client inside callback
// socket_callback takes socket as an argument
p2p_client.prototype.start = function(init_callback, socket_callback) {
  var _this = this;
  this.socket_callback = socket_callback;
  getGlobalSelfIpAddr(function(address) {
    _this.selfGlobalIpAddr = address;
    _this.selfGlobalUri = "http://" + _this.selfGlobalIpAddr + ':' +
      _this.http_port;
    _this.addSocketByUri(_this.selfLocalUri);
    _this.addSocketByUri(_this.otherServerUri);
    init_callback();
  });
}

p2p_client.prototype.initSocket = function(socket) {
  var _this = this;
  socket.on('connect', function() {
    // send global uri to otherServer
    socket.emit('add_user_client_to_server', _this.selfGlobalUri);
    // connect to given server uri and tell server's client to connect back
    socket.on('add_user_server_to_client', function(userGlobalUri) {
      _this.addSocketByUri(userGlobalUri, _this.socketTable).emit(
        'tell_your_client_to_connect_to_this_server', _this.selfGlobalUri
      );
    });
    // just connect to given server uri
    socket.on('just_connect_to_this_server', function(userGlobalUri) {
      _this.addSocketByUri(userGlobalUri, _this.socketTable);
    });
    socket.on('disconnect', function() {
      _this.removeSocket(socket, _this.socketTable);
    });
    socket.on('reconnect', function() {
      _this.addSocket(socket, _this.socketTable);
    });
    _this.socket_callback(socket);
  });
  return socket;
}

p2p_client.prototype.addSocket = function(socket) {
  this.socketTable.put(p2p_client.getUriOfSocket(socket), socket);
  return this.initSocket(this.socketTable.get(p2p_client.getUriOfSocket(
    socket)));
}

p2p_client.prototype.addSocketByUri = function(Uri) {
  if (Uri == this.selfGlobalUri) {
    var ret = this.socketTable.get(this.selfLocalUri);
    if (ret) {
      return ret;
    } else {
      return this.addSocket(client_io(this.selfLocalUri), this.socketTable);
    }
  } else {
    var ret = this.socketTable.get(Uri);
    if (!ret) {
      return this.addSocket(client_io(Uri), this.socketTable);
    } else {
      return ret;
    }
  }
}

p2p_client.prototype.removeSocket = function(socket) {
  return this.socketTable.remove(p2p_client.getUriOfSocket(socket));
}


function p2p_server(http_port) {
  this.http_port = http_port;
  this.selfLocalIpAddr = "127.0.0.1";
  this.selfLocalUri = "http://" + this.selfLocalIpAddr + ':' +
    this.http_port;
}

p2p_server.prototype.start = function(init_callback, socket_callback) {
  var _this = this;
  http.listen(this.http_port, init_callback);
  io.on('connection', function(socket) {
    if (p2p_server.getUriOfSocket(socket) == _this.selfLocalUri) {
      _this.localClientSocket = socket;
    }
    // send uri of client to all clients
    socket.on('add_user_client_to_server', function(userGlobalIpAddr) {
      io.emit('add_user_server_to_client', userGlobalIpAddr);
    });
    socket.on('tell_your_client_to_connect_to_this_server',
      function(userGlobalUri) {
        _this.localClientSocket.emit('just_connect_to_this_server',
          userGlobalUri);
      });
    socket.on('disconnect',
      function() {
        console.log("user " + p2p_server.getUriOfSocket(socket) +
          " disconnected :(");
      });
    socket.on('reconnect',
      function() {
        console.log("user " + p2p_server.getUriOfSocket(socket) +
          " reconnected :DDD");
      });
    socket_callback(socket);
  });
};

p2p_server.getUriOfSocket = function(socket) {
  return "http://" + socket.handshake.headers.host;
}

p2p_server.prototype.emit = function(event, data) {
  io.emit(event, data);
};


function p2p_peer(initialServerUri, http_port) {
  this.client = new p2p_client(initialServerUri, http_port);
  this.server = new p2p_server(http_port);
}

p2p_peer.prototype.start = function(client_init_callback,
  client_socket_callback,
  server_init_callback,
  server_socket_callback) {
  this.client.start(client_init_callback, client_socket_callback);
  this.server.start(server_init_callback, server_socket_callback);
}

p2p_peer.prototype.emit = function(event, data) {
  this.server.emit(event, data);
}

p2p_peer.prototype.addSocketByUri = function(Uri) {
  this.client.addSocketByUri(Uri);
}

p2p_peer.prototype.removeSocket = function(socket) {
  this.client.removeSocket(socket);
}

// helper functions
function getGlobalSelfIpAddr(callback) {
  dns.lookup(os.hostname(), function(error, address, family) {
    callback(address);
  });
}

module.exports = {
  peer: p2p_peer
}
