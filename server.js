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

utilities.fs.exists(activeFileName,function(exists){
    if (!exists){
        // make it exist, but blank
        utilities.fs.writeFileSync(activeFileName, "");
    }
    utilities.io.on('connection',function(socket){
        console.log('user ' + serverUserList.length + ' connected :)');
        // send user id and filename
        socket.emit('connection_info', {
            userId: serverUserList.length,
            activeFileName: activeFileName
        });
        // send file
        socket.emit('file_text',
                    utilities.fs.readFileSync(activeFileName).toString());
        socket.on('disconnect', function(){
            console.log('a user disconnected :(');
        });
    });
    utilities.http.listen(utilities.HTTP_PORT, function(){
        console.log("listening on " + utilities.os.hostname() + ":" +
                    utilities.HTTP_PORT);
    });
});
