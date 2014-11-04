var utilities = require('./utilities');
var p2p = require('./p2p_socket_io');

// argv[2] (required): the ipv4 address of the server

var otherServerUri = "http://" + process.argv[2] + ':' +
  utilities.SERVER_HTTP_PORT;
var p = new p2p.p2p_client(otherServerUri, utilities.SERVER_HTTP_PORT);

var fileContentsClient;
var activeFileNameClient;
p.start(function() {}, function(socket) {
  // get user id, filename, and file contents; write to file
  socket.on('connection_info', function(info) {
    activeFileNameClient = info.activeFileName +
      utilities.CLIENT_COPY_FILE_SUFFIX;
    fileContentsClient = info.fileContents;
    utilities.fs.writeFile(activeFileName, fileContentsClient);
  });
  socket.on('file_patch', function(patch) {
    utilities.fs.readFile(activeFileName,
      function(error, fileContentsClient) {
        utilities.fs.writeFile(activeFileName,
          utilities.diff_match_patch.patch_apply(patch,
            fileContentsClient)[0]);
      });
  });
  socket.on('file_in_full', function(fileContents) {
    utilities.fs.writeFile(activeFileName, fileContentsClient);
  });
});
