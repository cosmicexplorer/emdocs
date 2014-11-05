// includes
var fs = require('fs');
var os = require('os');
var spawn = require('child_process').spawn
var diff_match_patch = require('./diff_match_patch');
var p2p = require('./p2p_socket_io');
// constants
const SERVER_HTTP_PORT = 8080;
const DIFF_SYNC_TIME = 25; // in milliseconds
const FILE_SYNC_TIME = 500;
const TMP_DIFF_FILE_SUFFIX = ".tmp";
const CLIENT_COPY_FILE_SUFFIX = "_CLIENT_COPY";

// split into declarations and exports because variables need to be declared and
// initialized in sequence, and exports doesn't do that
module.exports = {
  fs: fs,
  os: os,
  spawn: spawn,
  diff_match_patch: diff_match_patch,
  p2p: p2p,
  SERVER_HTTP_PORT: SERVER_HTTP_PORT,
  DIFF_SYNC_TIME: DIFF_SYNC_TIME,
  FILE_SYNC_TIME: FILE_SYNC_TIME,
  TMP_DIFF_FILE_SUFFIX: TMP_DIFF_FILE_SUFFIX,
  CLIENT_COPY_FILE_SUFFIX: CLIENT_COPY_FILE_SUFFIX,
}
