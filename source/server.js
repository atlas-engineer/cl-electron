// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

////////////////////////////////////////////////////////////////////////
// Start a Javascript server that will eval code received.
////////////////////////////////////////////////////////////////////////

const path = require('node:path')
const nodejs_net = require('node:net');
const fs = require('node:fs');
const emitter = require('node:events');
const SynchronousSocket = require('synchronous-socket');
const { protocol } = require('electron')

// Eval and register protocols before we start Electron.
eval(process.argv.at(-1));

const { app, ipcMain, BrowserWindow, WebContentsView, webContents, net, dialog } = require('electron')

// Handle long messages from a socket and combine them into a single message.
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
    send(message) {
        this.socket.write(message + '\n');
    }
}
app.on('ready', () => {
    const server = nodejs_net.createServer((socket) => {
        const protocolSocket = new ProtocolSocket(socket, (message) => {
            try {
                const result = eval(message);
                protocolSocket.send(String(result));
            } catch (err) {
                protocolSocket.send(String(err));
            }
        });
    });

    server_socket_path = process.argv.at(-2);
    server.listen(server_socket_path, () => {
        fs.chmodSync(server_socket_path, 0o600)
    });
});

// Disable error dialogs.
dialog.showErrorBox = function(title, content) {
    console.log(`${title}\n${content}`);
};

// Do not limit the amount of possible listeners.
emitter.setMaxListeners(0)

// Generate Unique IDs for variable names.
var GLOBALS = {};
var uid = (function() {
  var id = 0;
  return function() {
    if (arguments[0] === 0) id = 0;
    return id++;
  }
})();


