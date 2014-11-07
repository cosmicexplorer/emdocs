var app = require('express')();
var http = require('http').Server(app);
var hash_table = require('hashtable');
var dns = require('dns');
var os = require('os');
var io = require('socket.io')(http);
var client_io = require('socket.io-client');

const global_http_port = 8080;


function p2p_client(initialServerUri) {
  this.otherServerUri = initialServerUri;
  this.http_port = global_http_port;
  this.selfLocalIpAddr = "127.0.0.1";
  this.selfLocalUri = "http://" + this.selfLocalIpAddr + ':' +
    global_http_port;
  this.socketTable = new hash_table();
}

p2p_client.getUriOfSocket = function(socket) {
  // console.log(socket);
  return socket.io.uri;
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
    // TODO: determine whether below line is required
    _this.addSocketByUri(_this.selfLocalUri, false);
    _this.addSocketByUri(_this.otherServerUri, true);
    if (typeof init_callback == "function") {
      init_callback();
    }
  });
}

p2p_client.prototype.getNumUsers = function() {
  return this.socketTable.size();
}

p2p_client.prototype.initSocket = function(socket, isAddNew) {
  var _this = this;
  socket.on('connect', function() {
    if (isAddNew) {
      // send global uri to otherServer
      p2p_client.broadcastAddThisUri(socket, _this.selfGlobalUri);
    }
    // connect to given server uri and tell server's client to connect back
    socket.on('add_this_server', function(userGlobalUri) {
      var sock = _this.addSocketByUri(userGlobalUri, false);
      setTimeout(function(){
        sock.emit('tell_attached_client_to_add_back', _this.selfGlobalUri);
      }, 1000);
      // _this.addSocketByUri(userGlobalUri, false).emit(
      //   'tell_attached_client_to_add_back', _this.selfGlobalUri);
      console.log("---");
      console.log("add_this_server received: " + userGlobalUri);
      console.log("tell_attached_client_to_add_back sent from: " +
        _this.selfGlobalUri + " to: " + userGlobalUri);
    });
    // just connect to given server uri
    socket.on('just_add_this_server', function(userGlobalUri) {
      _this.addSocketByUri(userGlobalUri, false);
      console.log("---");
      console.log("just_add_this_server received: " + userGlobalUri);
      console.log(userGlobalUri + " socket added")
    });
    socket.on('disconnect', function() {
      _this.removeSocket(socket);
    });
    socket.on('reconnect', function() {
      _this.addSocket(socket, false);
    });
    if (typeof _this.socket_callback == "function") {
      _this.socket_callback(socket);
    }
  });
  return socket;
}

// also serves to rejigger connections for robustness
p2p_client.broadcastAddThisUri = function(socket, selfGlobalUri) {
  socket.emit('tell_your_clients_to_add_me', selfGlobalUri);
  console.log("---");
  console.log("tell_your_clients_to_add_me sent: " + selfGlobalUri);
}

p2p_client.prototype.addSocket = function(socket, isAddNew) {
  this.socketTable.put(p2p_client.getUriOfSocket(socket), socket);
  return this.initSocket(this.socketTable.get(p2p_client.getUriOfSocket(
    socket)), isAddNew);
}

p2p_client.prototype.addSocketByUri = function(Uri, isAddNew) {
  if (Uri == this.selfGlobalUri) {
    var ret = this.socketTable.get(this.selfLocalUri);
    if (ret) {
      return ret;
    } else {
      return this.addSocket(client_io(this.selfLocalUri), isAddNew);
    }
  } else {
    var ret = this.socketTable.get(Uri);
    if (!ret) {
      return this.addSocket(client_io(Uri), isAddNew);
    } else {
      return ret;
    }
  }
}

p2p_client.prototype.removeSocket = function(socket) {
  return this.socketTable.remove(p2p_client.getUriOfSocket(socket));
}


function p2p_server() {
  this.http_port = global_http_port;
  this.selfLocalIpAddr = "127.0.0.1";
  this.selfLocalUri = "http://" + this.selfLocalIpAddr + ':' + this.http_port;
  this.socketTable = new hash_table();
}

p2p_server.prototype.start = function(init_callback, socket_callback) {
  var _this = this;
  http.listen(this.http_port, init_callback);
  io.on('connection', function(socket) {
    _this.socketTable.put(p2p_server.getUriOfSocket(socket), socket);
    console.log("user " + p2p_server.getUriOfSocket(socket) +
      " connected :)");
    if (p2p_server.getUriOfSocket(socket) == _this.selfLocalUri) {
      _this.localClientSocket = socket;
    }
    // send uri of client to all clients
    socket.on('tell_your_clients_to_add_me', function(userGlobalUri) {
      io.emit('add_this_server', userGlobalUri);
      console.log("---");
      console.log("tell_your_clients_to_add_me received: " +
        userGlobalUri);
      console.log("add_this_server sent: " + userGlobalUri);
    });
    socket.on('tell_attached_client_to_add_back',
      function(userGlobalUri) {
        _this.localClientSocket.emit('just_add_this_server',
          userGlobalUri);
        console.log("---");
        console.log(
          "tell_attached_client_to_add_back received: " +
          userGlobalUri);
        console.log("just_add_this_server sent: " + userGlobalUri);
      });
    socket.on('disconnect',
      function() {
        console.log("user " + p2p_server.getUriOfSocket(socket) +
          " disconnected :(");
        _this.socketTable.remove(p2p_server.getUriOfSocket(socket));
      });
    socket.on('reconnect',
      function() {
        console.log("user " + p2p_server.getUriOfSocket(socket) +
          " reconnected :DDD");
        _this.socketTable.put(p2p_server.getUriOfSocket(socket), socket);
      });
    if (typeof socket_callback == "function") {
      socket_callback(socket);
    }
  });
};

// TODO: figure out how to get client socket's http port!
p2p_server.getUriOfSocket = function(socket) {
  return "http://" + socket.handshake.address + ':' + global_http_port;
}

p2p_server.prototype.emit = function(event, data) {
  io.emit(event, data);
};


function p2p_peer(initialServerUri) {
  this.client = new p2p_client(initialServerUri);
  this.server = new p2p_server();
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

p2p_peer.prototype.getNumUsers = function() {
  return this.client.getNumUsers();
}

// helper functions
function getGlobalSelfIpAddr(callback) {
  dns.lookup(os.hostname(), function(error, address, family) {
    callback(address);
  });
}

module.exports = {
  http_port: global_http_port,
  client: p2p_client,
  server: p2p_server,
  peer: p2p_peer
}
