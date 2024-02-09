// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

/////////////////////////////////////////////////////////////
// Start a Javascript server that will eval code received. //
/////////////////////////////////////////////////////////////

if (process.argv.length != 3) {
  console.error('Requires server socket path.');
  process.exit(1);
}

const { app, ipcMain, BrowserView, BrowserWindow, webContents, protocol, net } = require('electron')
const path = require('path')
const nodejs_net = require('net');

const server = nodejs_net.createServer((socket) => {
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
