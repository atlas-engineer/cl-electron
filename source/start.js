const { app, ipcMain, BrowserWindow } = require('electron')
const path = require('path')
const net = require('net');

const server = net.createServer((socket) => {
    console.log('Client connected');
    socket.on('data', (data) => {
        try {
            const result = eval(data.toString());
            console.log('Result:', result);
            socket.write(`${result}\n`);
        } catch (err) {
            console.log('Error:', err);
            socket.write(`${err}\n`);
        }
    });
    socket.on('close', () => {
        console.log('Client disconnected');
    });
});
server.listen(3000, () => {
    console.log('Server listening on port 3000');
});

app.whenReady().then(() => {
    const win = new BrowserWindow({ width: 800, height: 600 })
    const socket = new net.Socket();
    socket.connect(3001, 'localhost', () => {
        socket.setNoDelay(true);
    });

    win.loadURL('https://atlas.engineer')
    win.webContents.on('before-input-event', (event, input) => {
        if (input.control && input.key.toLowerCase() === 'i') {
            socket.write('Hello, server!\n');
            event.preventDefault()
        }
    })
})
