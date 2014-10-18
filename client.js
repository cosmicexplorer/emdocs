var utilities = require('./utilities.js');

// argv[2] (required): the connection address of the server mediating the collab

var serverIpAddr = process.argv[2];
var serverUri = 'http://' + serverIpAddr + ':' + utilities.HTTP_PORT;

var socket = utilities.client_io(serverUri);

var userId;
var activeFileName;
socket.on('connect', function(){
    // get user id and filename
    socket.on('connection_info', function(info){
        userId = info.userId;
        activeFileName = info.activeFileName + "_CLIENT_COPY";
        socket.on('file_text', function(fileText){
            utilities.fs.writeFileSync(activeFileName, fileText);
        });
    });
});

// userIdAndFileLock.writeLock(function(release){
//     utilities.request
//         .get(serverUri)
//         .on('response', function(responseObject){
//             responseObject.on('data', function(chunk){
//                 var responseJSON = JSON.parse(chunk);
//                 userId = responseJSON.userId;
//                 activeFileName = responseJSON.activeFileName;
//                 console.log(userId);
//                 // file guaranteed not to exist, so writes to file
//                 utilities.fs.writeFile(
//                     activeFileName + "_CLIENT_COPY",
//                     responseJSON.fileContents,
//                     function(error){
//                         if (error){
//                             console.log("initial file write failed!");
//                             console.log(error);
//                         }
//                     });
//             });
//         });
//     release();
// });

// var fileWatcher;
// userIdAndFileLock.readLock(function(release){
//     var fileWatcher =
//         utilities.fs.watch(
//             activeFileName,
//             function(event, filename){
//                 if (event == "rename"){
//                     activeFileName = filename;
//                 } else {                    // event == "change"
//                     // TODO: add timeout
//                     utilities.request.post(
//                         serverIpAddr,
//                         { json: { "userId": userId } },
//                         function(error, responseObject, body){
//                             if (error || responseObject.statusCode !=
//                                 utilities.HTTP_RESPONSE_OK){
//                                 console.log(
//                                     "response failed in file watch post!");
//                                 console.log(response);
//                             }});
//                 }
//             });
//     release();
// });
