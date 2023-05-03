const { app, ipcMain, BrowserWindow } = require('electron')
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
server.listen(3000);

////////////
// Client //
////////////
client.connect(3001, 'localhost', () => {
    client.setNoDelay(true);
});
