import socket
import sys

# Create a TCP/IP socket
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

# Bind the socket to the port
server_address = ('localhost', 10001)
sock.bind(server_address)

# Listen for incoming connections
sock.listen(1)

while True:
    # Wait for a connection
    connection, client_address = sock.accept()
    try:
        while True:
            data = connection.recv(16)
            print(data.decode('utf-8'), file = sys.stderr);
            if data:
                connection.sendall(data)
            else:
                break

    finally:
        # Clean up the connection
        connection.close()
