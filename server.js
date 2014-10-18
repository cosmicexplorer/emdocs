var utilities = require('./utilities.js');

// argv[2] (required): the name of the active file

var activeFileName = process.argv[2];

function ServerUser(userId, ipAddr){
    this.id = userId;
    this.ipAddr = ipAddr;
}

var serverUserList = [];

// sets up queue of diffs
function addToQueue(counter, callback){
    console.log("adding user " + counter);
    callback();
}

// single worker because want to only process single user at a time
var userDiffQueue = utilities.async.queue(addToQueue,1);

var canonFileContents;
utilities.fs.exists(activeFileName,function(exists){
    if (!exists){
        // make it exist, but blank
        utilities.fs.writeFileSync(activeFileName, "");
    }
    canonFileContents = utilities.fs.readFileSync(activeFileName).toString();
    utilities.http.listen(utilities.HTTP_PORT, function(){
        console.log("listening on " + utilities.os.hostname() + ":" +
                    utilities.HTTP_PORT);
    });
});


// TODO: create list of users that can be added to and removed from atomictally
// at all times. hash table? atomic get/set operations library? idk. let's think
// about it on monday.
utilities.io.on('connection',function(socket){
    var userId = serverUserList.length;
    console.log('user ' + userId + ' connected :)');
    // send user id, filename, and file contents
    socket.emit('connection_info', {
        userId: userId,
        activeFileName: activeFileName,
        fileContents: canonFileContents
    });
    socket.on('disconnect', function(){
        console.log('user ' + userId + ' disconnected :(');
    });
});
