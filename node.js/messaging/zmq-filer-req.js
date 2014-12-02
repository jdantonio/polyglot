#!/usr/bin/env node --harmony

'use strict';

const zmq = require('zmq');

const filename = process.argv[2] || 'target.txt';

// request endpoint
const requester = zmq.socket('req');

// handle replies
requester.on('message', function(data) {
  let response = JSON.parse(data);
  console.log('Received response: ', response);
});

requester.connect('tcp://localhost:5433');

// send request
for (let i = 1; i <= 3; i++) {
  console.log('Sending request ' + i + ' for ' + filename);
  requester.send(JSON.stringify({
    path: filename
  }));
}