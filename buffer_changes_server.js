var async = require('async');   // for async queue
var fs = require('fs');

// // sets up queue of diffs
// function add_to_queue(counter, callback){
//     console.log("adding user " + counter);
//     callback();
// }

// var user_diff_queue = async.queue(add_to_queue,1);

// // sets up active file
// if (fs.existsSync(process.argv[1])){

// }

// for (var i = 0; i < 5; ++i){
//     console.log("i = " + i);
//     user_diff_queue.push(i, function(){
//         console.log("uploading " + i + " maybe??")
//     });
// }

var util  = require('util'),
spawn = require('child_process').spawn,
ps    = spawn('ps', ['aux']),
head  = spawn('head', ['-n15']);

ps.stdout.on('data', function (data) {
    head.stdin.write(data);
});

ps.stderr.on('data', function (data) {
    console.log('ps stderr: ' + data);
});

ps.on('exit', function (code) {
    if (code !== 0) {
        console.log('ps process exited with code ' + code);
    }
    head.stdin.end();
});

head.stdout.on("data", function(data){
    console.log(data);
});

head.stderr.on("data", function(data){
    console.log("head stderr " + data);
});

head.on("exit", function(code){
    if (code !== 0){
        console.log("head exited with code: " + code);
    }
});
