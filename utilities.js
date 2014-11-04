// includes
var app = require('express')();
var client_io = require('socket.io-client');
var fs = require('fs');
var http = require('http').Server(app);
var io = require('socket.io')(http);
var os = require('os');
var dns = require('dns');
var spawn = require('child_process').spawn
var diff_match_patch = require('./diff_match_patch');
var hash_table = require('hashtable');
// constants
const SERVER_HTTP_PORT = 8080;
const HTTP_RESPONSE_OK = 200;
const DIFF_SYNC_TIME = 250; // in milliseconds
const FILE_SYNC_TIME = 5000;
const TMP_DIFF_FILE_SUFFIX = ".tmp";
const CLIENT_COPY_FILE_SUFFIX = "_CLIENT_COPY";
// functions
function getGlobalSelfIpAddr(callback) {
  dns.lookup(os.hostname(), function(error, address, family) {
    callback(address);
  });
}

function getUriOfSocketAsClient(socket) {
  return "http://" + socket.io.opts.host + ':' + socket.io.opts.port;
}

function getUriOfSocketAsServer(socket) {
  return "http://" + socket.handshake.headers.host;
}

// split into declarations and exports because variables need to be declared and
// initialized in sequence, and exports doesn't do that
module.exports = {
  app: app,
  client_io: client_io,
  fs: fs,
  http: http,
  io: io,
  os: os,
  dns: dns,
  spawn: spawn,
  diff_match_patch: diff_match_patch,
  hash_table: hash_table,
  SERVER_HTTP_PORT: SERVER_HTTP_PORT,
  HTTP_RESPONSE_OK: HTTP_RESPONSE_OK,
  DIFF_SYNC_TIME: DIFF_SYNC_TIME,
  FILE_SYNC_TIME: FILE_SYNC_TIME,
  TMP_DIFF_FILE_SUFFIX: TMP_DIFF_FILE_SUFFIX,
  CLIENT_COPY_FILE_SUFFIX: CLIENT_COPY_FILE_SUFFIX,
  getGlobalSelfIpAddr: getGlobalSelfIpAddr,
  getUriOfSocketAsClient: getUriOfSocketAsClient,
  getUriOfSocketAsServer: getUriOfSocketAsServer
}
