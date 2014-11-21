// includes
var fs = require('fs');
var os = require('os');
var spawn = require('child_process').spawn
var diff_match_patch = require('./diff_match_patch');
var p2p = require('./p2p');
var locks = require('locks');
// constants
const SERVER_HTTP_PORT = 8080;
const DIFF_SYNC_TIME = 1000; // in milliseconds
const FILE_SYNC_TIME = 10000;
const TMP_FILENAME_SUFFIX = ".tmp";
const DIFF_FILENAME_SUFFIX = ".diff";
const CLIENT_COPY_FILE_SUFFIX = "_CLIENT_COPY";
const LISP_FILE_PATH = "./emacs_integration.el";

// split into declarations and exports because variables need to be declared and
// initialized in sequence, and exports doesn't do that
module.exports = {
  fs: fs,
  os: os,
  spawn: spawn,
  diff_match_patch: diff_match_patch,
  p2p: p2p,
  locks: locks,
  SERVER_HTTP_PORT: SERVER_HTTP_PORT,
  DIFF_SYNC_TIME: DIFF_SYNC_TIME,
  FILE_SYNC_TIME: FILE_SYNC_TIME,
  TMP_FILENAME_SUFFIX: TMP_FILENAME_SUFFIX,
  DIFF_FILENAME_SUFFIX: DIFF_FILENAME_SUFFIX,
  CLIENT_COPY_FILE_SUFFIX: CLIENT_COPY_FILE_SUFFIX,
  LISP_FILE_PATH: LISP_FILE_PATH
}
