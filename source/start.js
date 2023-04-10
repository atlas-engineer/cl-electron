const { app, BrowserWindow } = require('electron')
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
            socket.write(`Result: ${result}\n`);
        } catch (err) {
            console.log('Error:', err);
            // Send back the error message to the client
            socket.write(`Error: ${err}\n`);
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
