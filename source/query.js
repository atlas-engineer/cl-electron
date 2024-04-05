///////////////////////////////////////////////////////////////////
// Worker Script reads from socket, and writes result to stdout. //
///////////////////////////////////////////////////////////////////

const nodejs_net = require('net');
var socket = new nodejs_net.Socket();

socket.on("data", data => {
    let dataString = data.toString();
    let transmissionEndIndex = dataString.indexOf('');
    if (transmissionEndIndex != -1) {
        process.stdout.write(dataString.substring(0, transmissionEndIndex));
        socket.destroy();
        process.exit();
    }
});

socket.connect(process.env.SOCKET_PATH, () => {
    socket.setNoDelay(true);
    socket.write(process.env.SOCKET_DATA + '\n');
});
