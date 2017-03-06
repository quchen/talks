# Client-Server example program

This is a small example program to show a simple IO-heavy application.



## Server

The server waits for clients to connect. When a client sends a message, the
server will check whether the message is a number and add a special note if it
is prime. Other messages are simply printed in verbatim.

To start the server, run

```
stack build --exec simple-server
```



## Client

The client connects to a server, and sends linewise input to it. Clients can
quit with "quit".

To start a single client, run

```
stack build --exec simple-client
```
