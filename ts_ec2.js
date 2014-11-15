var p2p = require('./p2p.js');

// process.argv[2]: the ip to connect to

// var p = new p2p.peer("http://" + process.argv[2] + ":8080", 8080);

var p = new p2p.peer("http://" + process.argv[2] + ":8080", 8080);
p.start(
  function() {
    setInterval(function() {
      console.log("ip: " + p.client.selfGlobalUri);
      console.log("----\nclient users:");
      // p.client.socketTable.forEach(function(socketUri) {
      p.client.socketTable.forEach(function(socketUri) {
        console.log("\t" + socketUri);
      });
    }, 10000);
    // DIRTY HACKS, SCREW EC2
  },
  null,
  function() {
    setInterval(function() {
      console.log("----\nserver users:");
      p.server.socketTable.forEach(function(socketUri) {
        console.log("\t" + socketUri);
      });
    }, 10000);
  },
  null);
