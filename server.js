var utilities = require('./utilities');

// argv[2] (required): the name of the active file
// argv[3] (required): the ipv4 address of the server to contact first

var activeFileName = process.argv[2];

var fileContentsServer;
var fileExists = true;

var otherServerUri = "http://" + process.argv[3] + ':' +
  utilities.SERVER_HTTP_PORT;
var p = new utilities.p2p.peer(otherServerUri, utilities.SERVER_HTTP_PORT);

var fileContentsClient;
var activeFileNameClient;
utilities.fs.readFile(activeFileName, function(error, fileContentsServer) {
  if (error && error.code == 'ENOENT' && error.errno == 34) {
    // if file not found
    utilities.fs.writeFileSync(activeFileName, "");
    fileContentsServer = "";
  } else if (error) {
    fileExists = false;
    console.log(error);
  }
  if (fileExists) {

    openFileEmacs(activeFileName);

    p.start(
      // client init function
      function() {},
      // client socket function
      function(socket) {
        // get user id, filename, and file contents; write to file
        socket.on('connection_info', function(info) {
          activeFileNameClient = info.activeFileName +
            utilities.CLIENT_COPY_FILE_SUFFIX;
          fileContentsClient = info.fileContents;
          utilities.fs.writeFile(activeFileNameClient,
            fileContentsClient);
        });
        socket.on('file_patch', function(patch) {
          utilities.fs.readFile(activeFileName,
            function(error, fileContentsClient) {
              utilities.fs.writeFile(activeFileNameClient,
                utilities.diff_match_patch.patch_apply(patch,
                  fileContentsClient)[0]);
            });
        });
        socket.on('file_in_full', function(fileContents) {
          // TODO: gotta have mutexes for these things, for real
          utilities.fs.writeFile(activeFileName, fileContentsClient);
        });
      },
      // server init function
      function() {
        console.log("listening on " + utilities.os.hostname() + ":" +
          utilities.SERVER_HTTP_PORT);
      },
      // server socket function
      function(socket) {
        socket.emit('connection_info', {
          activeFileName: activeFileName,
          fileContentsServer: fileContentsServer
        });
        setInterval(broadcastDiffIfChanged, utilities.DIFF_SYNC_TIME);
        setInterval(broadcastFile, utilities.FILE_SYNC_TIME);
      });
  }
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
      console.log(":DDDDDDDDDDDDDDDDDD");
    }
  });
}

var numberOfTimesEmitted = 0;


function broadcastDiffIfChanged() {
  // TODO: actually check if file was changed, and don't broadcast if not
  // in addition, for this to work, it'll need to save the output to the canon
  // file version whenever a diff is broadcasted (else it will keep broadcasting
  // the same diff and keep inserting/deleting the same thing from file)
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
  emacsWriteFile.on('exit', function(return_code, signal) {
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
