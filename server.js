var utilities = require('./utilities');

// argv[2] (required): the name of the active file
// argv[3] (required): the ipv4 address of the server to contact first

var activeFileName = process.argv[2];

var fileContentsServer;
var fileExists = true;

var otherServerUri = "http://" + process.argv[3] + ':' +
  utilities.SERVER_HTTP_PORT;
var p = new utilities.p2p.peer(otherServerUri, utilities.SERVER_HTTP_PORT);

utilities.fs.writeFile(activeFileName, "", function(error) {
  if (error) {
    throw err;
  }
  openFileEmacs(activeFileName);
  p.start(
    // client init function
    function() {
      console.log("speaking on " + utilities.os.hostname() + ':' +
        utilities.SERVER_HTTP_PORT);
    },
    // client socket function
    function(socket) {
      // get user id, filename, and file contents; write to file
      socket.on('connection_info', function(info) {
        activeFileName = info.activeFileName;
        fileContents = info.fileContents;
        utilities.fs.writeFile(
          activeFileName,
          fileContents,
          function(error) {
            if (error) {
              throw error;
            }
            sendFileToEmacs(activeFileName);
          });
      });
      socket.on('file_send', function(sentFileContents) {
        utilities.fs.writeFile(
          activeFileName,
          sentFileContents,
          function(error) {
            if (error) {
              throw error;
            }
            sendFileToEmacs(activeFileName)
          });
      });
    },
    // server init function
    function() {
      console.log("listening on " + utilities.os.hostname() + ':' +
        utilities.SERVER_HTTP_PORT);
      if ("127.0.0.1" == process.argv[3]) { // if initial server
        setInterval(broadcastFile, utilities.FILE_SYNC_TIME);
      }
    },
    // server socket function
    function(socket) {
      utilities.fs.readFile(
        activeFileName,
        function(error, readFileContents) {
          fileContents = fileContents;
          socket.emit('connection_info', {
            activeFileName: activeFileName,
            fileContents: fileContents
          });
        });
    });
});

// requires emacs to be open and server to be on
function openFileEmacs(filename) {
  emacsOpenFile = utilities.spawn('emacsclient', ['-n', filename]);
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

var numberOfTimesEmitted = 0;


function broadcastDiffIfChanged() {
  // TODO: actually check if file was changed, and don't broadcast if not
  // in addition, for this to work, it'll need to save the output to the canon
  // file version whenever a diff is broadcasted (else it will keep broadcasting
  // the same diff and keep inserting/deleting the same thing from file)
  console.log("CLIENTS: " + p.client.socketTable.size());
  // p.client.socketTable.forEach(function(c, key, value) {
  //   console.log("socket " + c);
  //   console.log("\tkey: " + key);
  //   console.log("\tvalue: " + value);
  // });
  // console.log(p.client.socketTable);
  evalArg = "(send-buffer-to-file \"" + activeFileName + "\" \"" +
    utilities.TMP_DIFF_FILE_SUFFIX + "\")";
  emacsWriteFile = utilities.spawn('emacsclient', ['-e', evalArg]);
  emacsWriteFile.stdout.on('data', function(data) {
    console.log("emacs stdout: " + data);
  });
  emacsWriteFile.stderr.on('data', function(data) {
    console.log("emacs stderr: " + data);
  });
  emacsWriteFile.on('exit', function(return_code, signal) {
    if (return_code != 0) {
      console.log("error: file could not be saved.");
    } else {
      console.log("diff broadcasted");
      utilities.fs.readFile(activeFileName, function(error,
        fileContentsServer) {
        // OPTIMIZATION: fix the asynchronicity of this nesting
        utilities.fs.readFile(activeFileName + utilities.TMP_DIFF_FILE_SUFFIX,
          function(tmpError, tmpFileContents) {
            utilities.fs.writeFile(activeFileName, tmpFileContents,
              function() {
                var patch = utilities.diff_match_patch
                  .patch_make(
                    fileContentsServer.toString(),
                    tmpFileContents.toString()
                  );
                p.emit('file_patch', patch);
                ++numberOfTimesEmitted;
                console.log("number of times emitted: " +
                  numberOfTimesEmitted);
                console.log(patch[0]);
              });
          });
      });
    }
  });
}


function broadcastFile() {
  evalArg = "(send-buffer-to-file \"" + activeFileName + "\" \"" +
    utilities.TMP_DIFF_FILE_SUFFIX + "\")";
  emacsWriteFile = utilities.spawn('emacsclient', ['-e', evalArg]);
  emacsWriteFile.stdout.on('data', function(data) {
    console.log("emacs stdout: " + data);
  });
  emacsWriteFile.stderr.on('data', function(data) {
    console.log("emacs stderr: " + data);
  });
  emacsWriteFile.on('exit', function(
    return_code, signal) {
    if (return_code != 0) {
      console.log("error: file could not be saved.");
    } else {
      console.log("file broadcasted");
      utilities.fs.readFile(activeFileName,
        function(error, fileContentsServer) {
          p.emit('file_in_full', fileContentsServer.toString());
        });
    }
  });
}
