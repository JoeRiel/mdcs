import socketserver

class MyTCPHandler(socketserver.BaseRequestHandler):
    """
    The RequestHandler class for our server.

    It is instantiated once per connection to the server, and must
    override the handle() method to implement communication to the
    client.
    """

    def handle(self):
        # self.request is the TCP socket connected to the client
        while True:
            self.data = self.request.recv(8)  #.strip()
            print("%s wrote:" % self.client_address[0])
            if self.data:
                print(self.data)
                # just send back the same data, but upper-cased
                self.request.send(self.data.upper())
            else:
                break

class MyTCPServer(socketserver.TCPServer):
    def __init__(self, host, port):
        socketserver.TCPServer.__init__(self, (host, port), MyTCPHandler)
        self.allow_reuse_address=True



if __name__ == "__main__":
    HOST, PORT = "localhost", 9999

    # Create the server, binding to localhost on port 9999
    server = MyTCPServer(HOST,PORT)

    # Activate the server; this will keep running until you
    # interrupt the program with Ctrl-C
    server.serve_forever()
