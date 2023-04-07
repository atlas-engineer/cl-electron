# Define variables
host="localhost"
port=3000
message="app.whenReady().then(() => { createWindow() })"

# Attempt to send the message to the socket server
echo $message > /dev/tcp/$host/$port
