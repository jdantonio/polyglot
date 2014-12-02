#!/usr/bin/env node --harmony

'use strict';

const
  fs = require('fs'),
  zmq = require('zmq');

const filename = process.argv[2] || 'target.txt';

// publisher endpoint
const publisher = zmq.socket('pub');

fs.watch(filename, function() {
  publisher.send(JSON.stringify({
    type: 'changed',
    file: filename,
    timestamp: Date.now()
  }));
});

// listen on TCP port 5432
publisher.bind('tcp://*:5432', function(err) {
  console.log('Listening for zmq subscribers...');
});
