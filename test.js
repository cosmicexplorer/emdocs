// var d = require("./diff_match_patch.js");
// var u = require("util");

// var p = d.patch_make(
//   "asdfasdfasdf\nasdfasdfasdf\nasdfasdfasdf\nRACECARS",
//   "asdfasdf\nasdfasdfasdfasdf\nJASFJ:LSAF\nasafasfsdfasefascf\nMOTORBIKES\nAJFJLDFJ");

// console.log(JSON.stringify(p));

// [{"diffs":[[1,"asdf"],[0,"\n"]],"start1":0,"start2":0,"length1":1,"length2":5}]

var utilities = require('./utilities');

performPatchFromFile("test_file", ".diff");

function performPatchFromFile(filename, suffix, callback) {
  var emacsPerformPatch = spawnEmacsCommand(
    "perform-patch-from-file", "\"" + filename + "\"", "\"" + suffix + "\""
  );
  setupEmacsSpawn(
    emacsPerformPatch,
    "error: patch could not be performed",
    "file loaded",
    callback);
}


function setupEmacsSpawn(spawnProcess, errorText, successText, callback) {
  spawnProcess.stdout.on('data', function(data) {
    console.log("emacs stdout: " + data);
  });
  spawnProcess.stderr.on('data', function(data) {
    console.log("emacs stderr: " + data);
  });
  spawnProcess.on('exit', function(return_code, signal) {
    if (return_code != 0) {
      console.log(errorText);
    } else {
      console.log(successText);
    }
    if ("function" == typeof(callback)) {
      callback();
    }
  });
}

// takes variable number of arguments
// returns spawn object
function spawnEmacsCommand() {
  var evalArg = "(";
  for (var i = 0; i < arguments.length; ++i) {
    evalArg += arguments[i] + " ";
  }
  evalArg += ")";
  return utilities.spawn('emacsclient', ['-e', evalArg]);
}
