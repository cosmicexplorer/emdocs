var utilities = require('./utilities.js');

// argv[2] (required): the connection address of the server mediating the collab

var serverIpAddr = process.argv[2];
var selfServerUri = 'http://127.0.0.1:' + utilities.SERVER_HTTP_PORT;
var serverUri = 'http://' + serverIpAddr + ':' + utilities.SERVER_HTTP_PORT;

var activeFileName;
var fileContents;

var sockets = [];

utilities.http.listen(utilities.CLIENT_HTTP_PORT, function() {
  console.log("listening on " + utilities.os.hostname() + ":" +
    utilities.CLIENT_HTTP_PORT);
});

addSocket(utilities.client_io(selfServerUri), sockets);
// addSocket(utilities.client_io(serverUri), sockets);


function addSocket(socket, socketArray) { // initializes socket too
  return initSocket(socketArray[socketArray.push(socket) - 1]);
}


function initSocket(socket) {
  socket.on('connect', function() {
    // get user id, filename, and file contents
    socket.on('connection_info', function(info) {
      activeFileName = info.activeFileName + utilities.CLIENT_COPY_FILE_SUFFIX;
      fileContents = info.fileContents;
      utilities.fs.writeFile(activeFileName, fileContents);
    });
    socket.on('file_patch', function(patch) {
      utilities.fs.readFile(activeFileName, function(error,
        fileContents) {
        utilities.fs.writeFile(activeFileName, utilities.diff_match_patch
          .patch_apply(patch, fileContents)[0]);
      });
    });
  });
  return socket;
}
