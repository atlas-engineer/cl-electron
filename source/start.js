const { app, ipcMain, BrowserWindow } = require('electron')
const path = require('path')
const net = require('net');

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
const socket = new net.Socket();
socket.connect(3001, 'localhost', () => {
    socket.setNoDelay(true);
});

app.whenReady().then(() => {
    const win = new BrowserWindow({ width: 800, height: 600 })
    win.loadURL('https://atlas.engineer')
    win.webContents.on('before-input-event', (event, input) => {
        if (input.control && input.key.toLowerCase() === 'i') {
            socket.write('Hello, server!\n');
            event.preventDefault()
        }
    })
})
