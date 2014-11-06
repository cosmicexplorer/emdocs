var p2p = require('./p2p.js');

// process.argv[2]: the ip to connect to

var p = new p2p.peer("http://" + process.argv[2] + ":8080", 8080);
p.start(
  function() {
    setInterval(function() {
      console.log("----\nusers:")
      p.client.socketTable.forEach(function(
        socketUri) {
        console.log("\t" + socketUri);
      });
    }, 100000)
  },
  null,
  null,
  null);