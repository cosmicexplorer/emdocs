var utilities = require('./utilities.js');

// argv[2] (required): the name of the active file

var activeFileName = process.argv[2];

var fileContents;
var fileExists = true;

utilities.fs.readFile(activeFileName, function(error, fileContents) {
  if (error && error.code == 'ENOENT' && error.errno == 34) {
    // if file not found
    utilities.fs.writeFileSync(activeFileName, "");
    fileContents = "";
  } else if (error) {
    fileExists = false;
    console.log(error);
  }
  if (fileExists) {
    utilities.http.listen(utilities.SERVER_HTTP_PORT, function() {
      console.log("listening on " + utilities.os.hostname() + ":" +
        utilities.SERVER_HTTP_PORT);
    });
    utilities.io.on('connection', function(socket) {
      console.log('user ' + socket.id + ' connected :)');
      socket.emit('connection_info', {
        activeFileName: activeFileName,
        fileContents: fileContents
      });
      socket.on('disconnect',
        function() {
          console.log('user ' + socket.id + ' disconnected :(');
        });
      socket.on('reconnect',
        function() {
          console.log('user ' + socket.id + ' reconnected :DDD');
        });
    });
    // setInterval(broadcastDiff, utilities.DORMANT_SYNC_TIME, socket);
    setInterval(broadcastDiff, utilities.DORMANT_SYNC_TIME);
  }
});

// function broadcastDiff(socket) {
function broadcastDiff() {
  // on readfile stuff
  // io.emit('broadcastDiff', )
  evalArg = "(ex-stuff \"" + activeFileName + "\")";
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
      console.log("let's do stuff!");
    }
  });
}
