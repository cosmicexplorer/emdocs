var utilities = require('./utilities');

// requires emacs to be open and server to be on
function openFileInEmacs(filename, callback) {
  var emacsOpenFile = utilities.spawn('emacsclient', ['-n', filename]);
  setupEmacsSpawn(
    emacsOpenFile,
    "error: file could not be loaded",
    "file loaded",
    function() {
      console.log("FILE OPENED :DDDDDDD");
      if ("function" == typeof(callback)){
        callback();
      }
    });
}

function setupNotifyOnKeypress(filename, callback){
  var emacsNotifyOnKeypress = spawnEmacsCommand(
    "emdocs-set-after-change-functions", "\"" + filename + "\"");
  setupEmacsSpawn(
    emacsNotifyOnKeypress,
    "error: notifications could not be loaded",
    "notifications loaded",
    function(){
      console.log("notifications set");
      if ("function" == typeof(callback)){
        callback();
      }
    }
  )
}

function broadcastBuffer() {
  var emacsWriteFile = spawnEmacsCommand(
    "send-buffer-to-file", "\"" + activeFileName + "\"",
    "\"" + activeFileName + utilities.TMP_FILENAME_SUFFIX + "\"");
  setupEmacsSpawn(
    emacsWriteFile,
    "error: buffer could not be saved",
    "file broadcasted",
    function() {
      utilities.fs.readFile(
        activeFileName + utilities.TMP_FILENAME_SUFFIX,
        function(error, fileContents) {
          if (error) {
            console.log(error);
            if (34 == error.errno && 'ENOENT' == error.code) {
              fileContents = "";
            }
          }
          p.emit('file_send', fileContents.toString());
        });
    });
}


function writeBufferToFile(bufferName, filePath, callback) {
  var emacsWriteFile = spawnEmacsCommand(
    "send-buffer-to-file", "\"" + bufferName + "\"",
    "\"" + filePath + "\"");
  setupEmacsSpawn(
    emacsWriteFile,
    "error: buffer could not be saved",
    "file broadcasted",
    function() {
      utilities.fs.readFile(
        activeFileName + utilities.TMP_FILENAME_SUFFIX,
        function(error, fileContents) {
          if (error) {
            console.log(error);
          }
          if ("function" == typeof(callback)) {
            callback();
          }
        });
    });
}


function broadcastDiff() {
  var emacsWriteFile = spawnEmacsCommand(
    "send-buffer-to-file", "\"" + activeFileName + "\"",
    "\"" + activeFileName + utilities.TMP_FILENAME_SUFFIX + "\"");
  setupEmacsSpawn(
    emacsWriteFile,
    "error: buffer could not be loaded",
    "diff broadcasted",
    function() {
      utilities.fs.readFile(
        activeFileName + utilities.TMP_FILENAME_SUFFIX,
        function(tmpError, tmpFileContents) {
          if (tmpError) {
            console.log(tmpError);
            if (34 == tmpError.errno && 'ENOENT' == tmpError.code) {
              tmpFileContents = "";
            }
          }
          utilities.fs.readFile(
            activeFileName,
            function(curError, curFileContents) {
              if (curError) {
                console.log(curError);
                if (34 == curError.errno && 'ENOENT' == curError.code) {
                  curFileContents = "";
                }
              }
              utilities.fs.writeFile(
                activeFileName,
                tmpFileContents,
                function(error) {
                  if (error) {
                    console.log(error);
                  }
                  p.emit('file_diff',
                    utilities.diff_match_patch.patch_make(
                      curFileContents.toString(),
                      tmpFileContents.toString()));
                }
              );
            });
        });
    });
}


function updateBufferInEmacs(bufferName, filePath, callback) {
  var emacsReadFile = spawnEmacsCommand(
    "read-buffer-from-file",
    "\"" + bufferName + "\"",
    "\"" + filePath + "\"");
  setupEmacsSpawn(
    emacsReadFile,
    "error: file could not be read",
    "file read",
    callback);
}


function loadEmacsLisp(filename, callback) {
  var emacsLoadFile = spawnEmacsCommand(
    "load-file", "\"" + filename + "\""
  );
  setupEmacsSpawn(
    emacsLoadFile,
    "error: file could not be loaded",
    "file loaded",
    callback);
}


function performPatchFromFile(filename, callback) {
  var emacsPerformPatch = spawnEmacsCommand(
    "perform-patch-from-file", "\"" + filename + "\"",
    "\"" + filename + utilities.DIFF_FILENAME_SUFFIX + "\""
  );
  setupEmacsSpawn(
    emacsPerformPatch,
    "error: patch could not be performed",
    "file loaded",
    callback);
}

// helper function
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

// helper function
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
