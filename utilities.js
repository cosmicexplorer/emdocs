// includes
var app = require('express')();
var async = require('async');   // for async queue
var client_io = require('socket.io-client');
var fs = require('fs');
var http = require('http').Server(app);
var io = require('socket.io')(http);
var os = require('os');
var rwlock = require('rwlock'); // for mutexes
// constants
const HTTP_PORT = 8080;
const HTTP_RESPONSE_OK = 200;

module.exports = {
  app : app,
  async : async,
  client_io : client_io,
  fs : fs,
  http : http,
  io : io,
  os : os,
  rwlock : rwlock,
  HTTP_PORT : HTTP_PORT,
  HTTP_RESPONSE_OK : HTTP_RESPONSE_OK
}
