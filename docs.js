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

utilities.fs.writeFile(
  activeFileName,
  fileContents,
  function(error) {
    if (error) {
      console.log(error);
    }
    utilities.fs.writeFile(
      activeFileName + utilities.TMP_FILENAME_SUFFIX,
      tmpFileContents,
      function(error) {
        if (error) {
          console.log(error);
        }
        openFileInEmacs(activeFileName);
        p.start(
          // client init function
          function() {
            console.log("speaking on " + utilities.os.hostname() + ':' +
              utilities.SERVER_HTTP_PORT);
          },
          // client socket function
          function(socket) {
            // get user id, filename, and file contents; write to file
            socket.on('connection_info', function(sentFileName) {
              activeFileName = sentFileName;
            });
            socket.on('file_send', function(sentFileContents) {
              // if not self socket
              if ("http://127.0.0.1:" + utilities.SERVER_HTTP_PORT !=
                utilities.p2p.client.getUriOfSocket(socket)) {
                utilities.fs.writeFile(
                  activeFileName,
                  sentFileContents.toString(),
                  function(error) {
                    if (error) {
                      console.log(error);
                    }
                    updateBufferInEmacs(activeFileName);
                  });
              }
            });
            socket.on('file_diff', function(sentFilePatch) {
              if ("http://127.0.0.1:" + utilities.SERVER_HTTP_PORT !=
                utilities.p2p.client.getUriOfSocket(socket)) {
                utilities.fs.readFile(
                  activeFileName,
                  function(error, readFileContents) {
                    utilities.fs.writeFile(
                      activeFileName,
                      utilities.diff_match_patch.patch_apply(
                        sentFilePatch,
                        readFileContents
                      )[0], // get patched text
                      function(error) {
                        if (error) {
                          console.log(error);
                        }
                        updateBufferInEmacs(activeFileName);
                      });
                  });
              }
            });
          },
          // server init function
          function() {
            console.log("listening on " + utilities.os.hostname() + ':' +
              utilities.SERVER_HTTP_PORT);
            if ("127.0.0.1" == process.argv[3]) { // if initial server
              setInterval(broadcastBuffer, utilities.FILE_SYNC_TIME);
              setInterval(broadcastDiff, utilities.DIFF_SYNC_TIME);
            }
          },
          // server socket function
          function(socket) {
            socket.emit('connection_info', activeFileName);
          });
      });
  });

// requires emacs to be open and server to be on
function openFileInEmacs(filename) {
  var emacsOpenFile = utilities.spawn('emacsclient', ['-n', filename]);
  emacsOpenFile.stdout.on('data', function(data) {
    console.log("emacs stdout: " + data);
  });
  emacsOpenFile.stderr.on('data', function(data) {
    console.log("emacs stderr: " + data);
  });
  emacsOpenFile.on('exit', function(return_code, signal) {
    if (return_code != 0) {
      console.log("error: file could not be opened.");
    } else {
      console.log("FILE OPENED :DDDDDDDDDDDDDDDDDD");
    }
  });
}


function broadcastBuffer() {
  var emacsWriteFile = spawnEmacsCommand(
    "send-buffer-to-file", "\"" + activeFileName + "\"",
    "\"" + utilities.TMP_FILENAME_SUFFIX + "\"");
  emacsWriteFile.stdout.on('data', function(data) {
    console.log("emacs stdout: " + data);
  });
  emacsWriteFile.stderr.on('data', function(data) {
    console.log("emacs stderr: " + data);
  });
  emacsWriteFile.on('exit', function(
    return_code, signal) {
    if (0 != return_code) {
      console.log("error: buffer could not be saved.");
    } else {
      utilities.fs.readFile(
        activeFileName + utilities.TMP_FILENAME_SUFFIX,
        function(error, fileContents) {
          if (error) {
            console.log(error);
          }
          p.emit('file_send', fileContents.toString());
          console.log("file broadcasted")
        });;
    }
  });
}


function broadcastDiff() {
  var emacsWriteFile = spawnEmacsCommand(
    "send-buffer-to-file", "\"" + activeFileName + "\"",
    "\"" + utilities.TMP_FILENAME_SUFFIX + "\"");
  emacsWriteFile.stdout.on('data', function(data) {
    console.log("emacs stdout: " + data)
  });
  emacsWriteFile.stderr.on('data', function(data) {
    console.log("emacs stderr: " + data);
  });
  emacsWriteFile.on('exit', function(return_code, signal) {
    if (0 != return_code) {
      console.log("error: buffer could not be saved.");
    } else {
      utilities.fs.readFile(
        activeFileName + utilities.TMP_FILENAME_SUFFIX,
        function(tmpError, tmpFileContents) {
          if (tmpError) {
            console.log(tmpError);
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
                  console.log(error);
                  p.emit('file_diff',
                    utilities.diff_match_patch.patch_make(
                      curFileContents.toString(), tmpFileContents.toString()));
                }
              )
            });
        });
    }
  });
}


function updateBufferInEmacs(filename) {
  var emacsReadFile = spawnEmacsCommand(
    "read-buffer-from-file", "\"" + filename + "\"");
  emacsReadFile.stdout.on('data', function(data) {
    console.log("emacs stdout: " + data);
  });
  emacsReadFile.stderr.on('data', function(data) {
    console.log("emacs stderr: " + data);
  });
  emacsReadFile.on('exit', function(return_code, signal) {
    if (return_code != 0) {
      console.log("error: file could not be read.");
    }
    console.log("file read");
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
