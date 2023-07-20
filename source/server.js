// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

/////////////////////////////////////////////////////////////
// Start a Javascript server that will eval code received. //
/////////////////////////////////////////////////////////////

if (process.argv.length != 4) {
  console.error('Requires server and client socket paths.');
  process.exit(1);
}

const { app, ipcMain, BrowserView, BrowserWindow } = require('electron')
const path = require('path')
const net = require('net');
client = new net.Socket();

////////////
// Server //
////////////
const server = net.createServer((socket) => {
    socket.on('data', (data) => {
        try {
            const result = eval(data.toString());
            socket.write(`${result}\n`);
        } catch (err) {
            socket.write(`${err}\n`);
        }
    });
});
server.listen(process.argv[2]);

////////////
// Client //
////////////
client.connect(process.argv[3], () => {
    client.setNoDelay(true);
});
