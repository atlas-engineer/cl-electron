///////////////////////////////////////////////////////////////////
// Worker Script reads from socket, and writes result to stdout. //
///////////////////////////////////////////////////////////////////

const nodejs_net = require('net');
var socket = new nodejs_net.Socket();

socket.on("data", data => {
    let dataString = data.toString();
    process.stdout.write(dataString);
    process.exit();
});

socket.connect(process.env.SOCKET_PATH, () => {
    socket.setNoDelay(true);
    socket.write(process.env.SOCKET_DATA + '\\n');
});

process.stdout.write(process.env.SOCKET_PATH);
process.exit();
