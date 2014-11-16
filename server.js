var utilities = require('./utilities');

// argv[2] (required): the name of the active file
// argv[3] (required): the ipv4 address of the server to contact first

var activeFileName = process.argv[2];

var fileContents;

var otherServerUri = "http://" + process.argv[3] + ':' +
  utilities.SERVER_HTTP_PORT;
var p = new utilities.p2p.peer(otherServerUri, utilities.SERVER_HTTP_PORT);

utilities.fs.writeFile(activeFileName, "", function(error) {
  if (error) {
    throw err;
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
        if ("http://127.0.0.1" + utilities.SERVER_HTTP_PORT !=
            utilities.p2p.client.getUriOfSocket(socket)) {
          utilities.fs.writeFile(
            activeFileName,
            sentFileContents,
            function() {
              updateBufferInEmacs(activeFileName);
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
      }
    },
    // server socket function
    function(socket) {
      socket.emit('connection_info', activeFileName);
    });
});

// requires emacs to be open and server to be on
function openFileInEmacs(filename) {
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


function broadcastBuffer() {
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
      // did not previously have the + utilities.TMP_DIFF_FILE_SUFFIX
      utilities.fs.readFile(
        activeFileName + utilities.TMP_DIFF_FILE_SUFFIX,
        function(error, fileContents) {
          if (error) {
            utilities.fs.writeFile(
              activeFileName + utilities.TMP_DIFF_FILE_SUFFIX,
              "",
              function() {
                p.emit('file_send', fileContents.toString());
              }
            );
          }
          p.emit('file_send', fileContents.toString());
        });
    }
  });
}


function updateBufferInEmacs(filename) {
  evalArg = "(read-buffer-from-file \"" + filename + "\")";
  emacsReadFile = utilities.spawn('emacsclient', ['-e', evalArg]);
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
