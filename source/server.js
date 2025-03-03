// SPDX-FileCopyrightText: Atlas Engineer LLC
// SPDX-License-Identifier: BSD-3-Clause

/////////////////////////////////////////////////////////////
// Start a Javascript server that will eval code received. //
/////////////////////////////////////////////////////////////

const path = require('node:path')
const nodejs_net = require('node:net');
const fs = require('node:fs');
const emitter = require('node:events');
// The architecture of protocol handling resorts to a tmp file, meaning that the
// main JS location may differ from the location of the current file.
const SynchronousSocket = require(path.resolve('node_modules/synchronous-socket'));
const { app, ipcMain, BrowserWindow, WebContentsView, webContents, protocol, net, dialog } = require('electron')

// Disable error dialogs
dialog.showErrorBox = function(title, content) {
    console.log(`${title}\n${content}`);
};

// Generate Unique IDs for variable names.
var GLOBALS = {};
var uid = (function() {
  var id = 0;
  return function() {
    if (arguments[0] === 0) id = 0;
    return id++;
  }
})();

emitter.setMaxListeners(0)

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
    server_socket_path = process.argv.at(-1);
    server.listen(server_socket_path, () => {
        fs.chmodSync(server_socket_path, 0o600)
    });
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
