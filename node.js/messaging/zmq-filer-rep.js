#!/usr/bin/env node --harmony

'use strict';

const
  fs = require('fs'),
  zmq = require('zmq');

// socket to reply to client requests
const responder = zmq.socket('rep');

// handle incoming requests
responder.on('message', function(data) {

  // parse incoming requests
  let request = JSON.parse(data);
  console.log('Received request to get: ' + request.path);

  // read file and reply
  fs.readFile(request.path, function(err, content) {
    let msg = '';

    if (err) {
      msg = JSON.stringify({
        error: err.code,
        filename: request.path,
        timestamp: Date.now(),
        pid: process.pid
      });
      console.log('Error processing request: ' + msg);
    } else {
      msg = JSON.stringify({
        content: content.toString(),
        timestamp: Date.now(),
        pid: process.pid
      });
      console.log('Sending response content: ' + msg);
    }
    responder.send(msg);
  });
});

// listen on TCP port 5433
responder.bind('tcp://127.0.0.1:5433', function(err) {
  console.log('Listening for zmq requesters...');
});

// close the responder when the Node process ends
process.on('SIGINT', function() {
  console.log('Shutting down...');
  responder.close();
});
