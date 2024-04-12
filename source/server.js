// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

/////////////////////////////////////////////////////////////
// Start a Javascript server that will eval code received. //
/////////////////////////////////////////////////////////////

if (process.argv.length != 3) {
  console.error('Server socket path required.');
  process.exit(1);
}

const SynchronousSocket = require('bindings')('SynchronousSocket');
const { app, ipcMain, BrowserView, BrowserWindow, webContents, protocol, net } = require('electron')
const path = require('path')
const nodejs_net = require('net');
const childProcess = require('child_process');

app.on('ready', () => {
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
});

////////////////////////////////////////////////////////////////////////////////
// Handle long messages from a socket and combine them into a single message. //
////////////////////////////////////////////////////////////////////////////////

class ProtocolSocket {
    constructor(socket, onDataFunction) {
        this.socket = socket;
        this.onDataFunction = onDataFunction;
        this.messageBuffer = '';

        this.socket.on('data', data => {
            let dataString = data.toString();
            let transmissionEndIndex = dataString.indexOf('');
            if (transmissionEndIndex == -1) {
                this.messageBuffer += dataString;
            } else {
                this.messageBuffer += dataString.substring(0, transmissionEndIndex);
                this.onDataFunction(this.messageBuffer);
                this.messageBuffer = dataString.substring(transmissionEndIndex + 1, dataString.length);
            }
        });
    }
}
