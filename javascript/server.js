var utilities = require('./utilities');
var serverFunctions = require('./server_functions');

// argv[2] (required): the name of the active file
// argv[3] (required): the ipv4 address of the server to contact first

var activeFileName = process.argv[2];

var fileContents = "";
var tmpFileContents = fileContents;

var otherServerUri = "http://" + process.argv[3] + ':' +
  utilities.SERVER_HTTP_PORT;

var p = new utilities.p2p.peer(otherServerUri, utilities.SERVER_HTTP_PORT);

// TODO: make proxy solution for network with firewall which blocks all ports
// TODO: make solution to just find empty port instead of uniformly using 8080

utilities.fs.watchFile(utilities.EMIT_FILENAME, function(curVer, prevVer) {
  if (curVer.mtime != prevVer.mtime) {
    utilities.fs.readFile(utilities.EMIT_FILENAME, function(error, contents) {
      console.log("INOTIFY CONTENTS:");
      console.log(contents.toString());
    });
  }
});

utilities.fs.writeFileSync(activeFileName, fileContents);
utilities.fs.writeFileSync(
  activeFileName + utilities.TMP_FILENAME_SUFFIX,
  tmpFileContents);

serverFunctions.openFileInEmacs(activeFileName, function() {
  serverFunctions.loadEmacsLisp(utilities.LISP_FILE_PATH, function() {
    serverFunctions.setupNotifyOnKeypress(activeFileName, function() {
      p.start(
        // client init function
        function() {
          console.log("speaking on " + utilities.os.hostname() +
            ':' +
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
                serverFunctions.updateBufferInEmacs(
                  activeFileName,
                  activeFileName
                );
                console.log("connection info received");

                socket.on('file_send', function(
                  sentFileBuffer) {
                  // if not self socket
                  if ("http://127.0.0.1:" + utilities.SERVER_HTTP_PORT !=
                    utilities.p2p.client.getUriOfSocket(
                      socket)) {
                    if (undefined == sentFileBuffer) {
                      sentFileBuffer = "";
                    }
                    utilities.fs.writeFile(
                      activeFileName,
                      sentFileBuffer,
                      function(error) {
                        if (error) {
                          console.log(error);
                        }
                        serverFunctions.updateBufferInEmacs(
                          activeFileName,
                          activeFileName
                        );
                        console.log("file received");
                      });
                  }
                });

                socket.on('file_diff', function(
                  sentFilePatch) {
                  // if not self socket
                  if ("http://127.0.0.1:" + utilities.SERVER_HTTP_PORT !=
                    utilities.p2p.client.getUriOfSocket(
                      socket)) {
                    serverFunctions.writeBufferToFile(
                      activeFileName,
                      activeFileName + utilities.DIFF_FILENAME_SUFFIX,
                      function() {
                        utilities.fs.readFile(
                          activeFileName +
                          utilities.DIFF_FILENAME_SUFFIX,
                          function(error,
                            readFileBuffer) {
                            if (error) {
                              if (34 == error.errno &&
                                'ENOENT' == error.code
                              ) {
                                readFileBuffer = "";
                              }
                              console.log(error);
                            }
                            var readFileContents =
                              readFileBuffer.toString();
                            readFileContents +=
                              "\n";
                            console.log(
                              sentFilePatch);
                            var patchResult =
                              utilities.diff_match_patch
                              .patch_apply(
                                sentFilePatch,
                                readFileContents()
                              )[0];
                            utilities.fs.writeFile(
                              activeFileName,
                              patchResult,
                              function(error) {
                                if (error) {
                                  console.log(
                                    error);
                                }
                                serverFunctions.updateBufferInEmacs(
                                  activeFileName,
                                  activeFileName
                                );
                                console.log(
                                  "diff received"
                                );
                              });
                          });
                      });
                  }
                });
              });
          });
        },

        // server init function
        function() {
          console.log("listening on " + utilities.os.hostname() +
            ':' +
            utilities.SERVER_HTTP_PORT);
          // if (process.argv[3] == "127.0.0.1") {
          //   setInterval(
          //     serverFunctions.broadcastBuffer,
          //     utilities.FILE_SYNC_TIME);
          // }
          // setInterval(serverFunctions.broadcastDiff, utilities.DIFF_SYNC_TIME);
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
});
