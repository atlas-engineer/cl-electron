const { app, ipcMain, BrowserWindow } = require('electron')
const path = require('path')
const net = require('net');

const server = net.createServer((socket) => {
    console.log('Client connected');

    // Handle incoming data
    socket.on('data', (data) => {
        // Evaluate incoming data as Javascript code
        try {
            const result = eval(data.toString());
            console.log('Result:', result);
            // Send back the result to the client
            socket.write(`${result}\n`);
        } catch (err) {
            console.log('Error:', err);
            // Send back the error message to the client
            socket.write(`${err}\n`);
        }
    });

    // Handle socket closed
    socket.on('close', () => {
        console.log('Client disconnected');
    });
});

server.listen(3000, () => {
    console.log('Server listening on port 3000');
});

// before-input-event
// https://www.electronjs.org/docs/latest/tutorial/keyboard-shortcuts

app.whenReady().then(() => {
  const win = new BrowserWindow({ width: 800, height: 600 })

    win.loadURL('https://atlas.engineer')
  win.webContents.on('before-input-event', (event, input) => {
    if (input.control && input.key.toLowerCase() === 'i') {
      console.log('Pressed Control+I')
      event.preventDefault()
    }
  })
})
