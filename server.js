var utilities = require('./utilities');

// argv[2] (required): the name of the active file
// argv[3] (required): the ipv4 address of the server to contact first

var activeFileName = process.argv[2];

var fileContents = "";
var tmpFileContents = fileContents;

var otherServerUri = "http://" + process.argv[3] + ':' +
  utilities.SERVER_HTTP_PORT;

var p = new utilities.p2p.peer(otherServerUri, utilities.SERVER_HTTP_PORT);

// TODO: make proxy solution for network with firewall which blocks all ports

utilities.fs.writeFileSync(activeFileName, fileContents);
utilities.fs.writeFileSync(
  activeFileName + utilities.TMP_FILENAME_SUFFIX,
  tmpFileContents);

loadEmacsLisp(utilities.LISP_FILE_PATH, function() {
  openFileInEmacs(activeFileName, function() {
    p.start(
      // client init function
      function() {
        console.log("speaking on " + utilities.os.hostname() + ':' +
          utilities.SERVER_HTTP_PORT);
      },

      // client socket function
      function(socket) {
        // get user id, filename, and file contents; write to file
        socket.on('connection_info', function(
          connectionInfo) {
          activeFileName = connectionInfo.activeFileName;
          fileContents = connectionInfo.fileContents;
          tmpFileContents = fileContents;
          utilities.fs.writeFile(
            activeFileName,
            fileContents,
            function(error) {
              if (error) {
                console.log(error);
              }
              updateBufferInEmacs(
                activeFileName,
                activeFileName + utilities.TMP_FILENAME_SUFFIX
              );
              console.log("connection info received");

              socket.on('file_send', function(sentFileBuffer) {
                // if not self socket
                if ("http://127.0.0.1:" + utilities.SERVER_HTTP_PORT !=
                  utilities.p2p.client.getUriOfSocket(
                    socket)) {
                  if (sentFileBuffer.toString() == "") {
                    console.log("WHAT");
                  }
                  utilities.fs.writeFile(
                    activeFileName + utilities.TMP_FILENAME_SUFFIX,
                    sentFileBuffer,
                    function(error) {
                      if (error) {
                        console.log(error);
                      }
                      updateBufferInEmacs(
                        activeFileName,
                        activeFileName + utilities.TMP_FILENAME_SUFFIX
                      );
                      console.log("file received");
                    });
                }
              });

              socket.on('file_diff', function(sentFilePatch) {
                if ("http://127.0.0.1:" + utilities.SERVER_HTTP_PORT !=
                  utilities.p2p.client.getUriOfSocket(
                    socket)) {
                  utilities.fs.writeFile(
                    activeFileName +
                    utilities.DIFF_FILENAME_SUFFIX,
                    JSON.stringify(sentFilePatch),
                    function(error) {
                      if (error) {
                        console.log(error);
                      }
                      console.log(
                        JSON.stringify(
                          sentFilePatch));
                      performPatchFromFile(
                        activeFileName,
                        activeFileName +
                        utilities.DIFF_FILENAME_SUFFIX
                      );
                      console.log("diff received");
                    });
                }
              });
            });
        });
      },

      // server init function
      function() {
        console.log("listening on " + utilities.os.hostname() + ':' +
          utilities.SERVER_HTTP_PORT);
        if (process.argv[3] == "127.0.0.1") {
          setInterval(broadcastDiff, utilities.DIFF_SYNC_TIME);
          setInterval(broadcastBuffer, utilities.FILE_SYNC_TIME);
        }
      },

      // server socket function
      function(socket) {
        socket.emit('connection_info', {
          activeFileName: activeFileName,
          fileContents: fileContents
        });
      });
  });
});

// requires emacs to be open and server to be on
function openFileInEmacs(filename, callback) {
  var emacsOpenFile = utilities.spawn('emacsclient', ['-n', filename]);
  setupEmacsSpawn(
    emacsOpenFile,
    "error: file could not be loaded",
    "file loaded",
    function() {
      console.log("FILE OPENED :DDDDDDD");
      callback();
    });
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


function writeBufferToFile(callback) {
  var emacsWriteFile = spawnEmacsCommand(
    "send-buffer-to-file", "\"" + activeFileName + "\"",
    "\"" + activeFileName + "\"");
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
            tmpFileContents = "";
          }
          utilities.fs.readFile(
            activeFileName,
            function(curError, curFileContents) {
              if (curError) {
                console.log(curError);
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
