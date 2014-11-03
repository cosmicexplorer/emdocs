// includes
var app = require('express')();
var client_io = require('socket.io-client');
var fs = require('fs');
var http = require('http').Server(app);
var io = require('socket.io')(http);
var os = require('os');
var spawn = require('child_process').spawn
var diff_match_patch = require('./diff_match_patch');
// constants
const SERVER_HTTP_PORT = 8080;
const CLIENT_HTTP_PORT = 3000;
const HTTP_RESPONSE_OK = 200;
const DORMANT_SYNC_TIME = 250; // in milliseconds
// const DORMANT_SYNC_TIME = 5000; // use this for release
const TMP_DIFF_FILE_SUFFIX = ".tmp";
const CLIENT_COPY_FILE_SUFFIX = "_CLIENT_COPY"

// split into declarations and exports because variables need to be declared and
// initialized in sequence, and exports doesn't do that
module.exports = {
  app: app,
  client_io: client_io,
  fs: fs,
  http: http,
  io: io,
  os: os,
  spawn: spawn,
  diff_match_patch: diff_match_patch,
  SERVER_HTTP_PORT: SERVER_HTTP_PORT,
  CLIENT_HTTP_PORT: CLIENT_HTTP_PORT,
  HTTP_RESPONSE_OK: HTTP_RESPONSE_OK,
  DORMANT_SYNC_TIME: DORMANT_SYNC_TIME,
  TMP_DIFF_FILE_SUFFIX: TMP_DIFF_FILE_SUFFIX,
  CLIENT_COPY_FILE_SUFFIX: CLIENT_COPY_FILE_SUFFIX
}
