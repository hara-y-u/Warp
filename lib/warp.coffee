#!/usr/bin/env coffee

http = require 'http'
url = require 'url'
WebSocketServer = new require('websocket').server
messageParser = /(\w+):(.+)/

PORT = 8898

# Static
CLIENT_HTML = '''
<!DOCTYPE html>
<html>
  <head>
    <title>Warp</title>
    <style>
      * { margin:0; padding:0 }
      header { height:1.2em; overflow:hidden; border-bottom:solid 1px #bbb; }
      body { height:100%; width:100%; }
      iframe#warp-frame { height:100%; width:100%; border:0; }
    </style>
    <script src="/client.js"></script>
  </head>
  <body>
    <header>
      Warp Client
    </header>
    <iframe id=\"warp-frame\" src="/content.html"/>
  </body>
</html>
'''

CLIENT_JS = """
(function () {

var soc = new WebSocket('ws://localhost:#{PORT}/', 'warp')
, nop = function(){}
, startupStack = []
;

startupStack.push(function() {
  soc.send(JSON.stringify({ type:'status', data:'start' }));

  var frame = document.getElementById('warp-frame');

  soc.onmessage = function(msg) {
    msg = JSON.parse(msg.data);
    switch (msg.type) {
      // case 'reload':
      //   frame.contentWindow.location.reload();
      //   break;
      case 'load':
      case 'url':
        frame.contentWindow.location.href = msg.data;
        break;
      case 'html':
        frame.contentDocument.documentElement.innerHTML = msg.data
          .replace(/<!doctype[^>]*>/i, '').replace(/<\\/?html[^>]*>/i, '');
        break;
      default:
        soc.send(JSON.stringify({ type:'error', data:'unknown_type' }));
    }
  };

});

startupStack.push(nop);
soc.onopen = function() { startupStack.pop()(); };

startupStack.push(nop);
document.addEventListener('DOMContentLoaded', function() { startupStack.pop()(); });

startupStack.pop()();

soc.onclose = function() {
  soc.send(JSON.stringify({ type:'status', data:'stop' }));
};

}());
"""

CONTENT_HTML = '''
<html>
  <body>
  </body>
</html>
'''

module.exports = class Warp
  constructor: (options = {}) ->
    @port   = options.port   or PORT
    @stdin  = process.stdin
    @stdout = process.stdout
    @stderr = process.stderr

  startServer: () =>
    @startHttpServer()
    @startWebSocketServer()
    @startStdinListener()

  startHttpServer: () =>
    @httpServer = http.createServer @handleHttpRequest
    @httpServer.listen @port
    @stdout.write "start:lotalhost:#{PORT}\n"

  handleHttpRequest: (req, res) =>
    switch url.parse(req.url).path
      when '/'
        res.writeHead 200, 'Content-Type': 'text/html'
        res.write CLIENT_HTML, 'utf-8'
      when '/content.html'
        res.writeHead 200, 'Content-Type': 'text/html'
        res.write CONTENT_HTML, 'utf-8'
      when '/client.js'
        res.writeHead 200, 'Content-Type': 'text/javascript'
        res.write CLIENT_JS, 'utf-8'
      else
        res.writeHead 404, 'Content-Type': 'text/plain'
        res.write '404 Not Found\n'

    res.end()

  startWebSocketServer: () =>
    # WebSocket
    @webSocketServer = new WebSocketServer
      httpServer: @httpServer

    @webSocketServer.on 'request', (req) =>
      @webSocket = req.accept 'warp', req.origin

      #From Client
      @webSocket.on 'message', (msg) =>
        msg = JSON.parse(msg.utf8Data);
        @handleWebSocketMessage msg

  handleWebSocketMessage: (msg) =>
    process.stdout.write "client_#{msg.type}:#{msg.data}\n"

  startStdinListener: () =>
    # STDIN
    @stdin.resume()
    @stdin.setEncoding 'utf8'
    @stdin.on 'data', @handleStdin
    @stdin.on 'end', () ->
      @stdout.write 'status:stdin_end\n'

  handleStdin: (chunk) =>
    msg = messageParser.exec chunk
    if !msg or !msg[1] or !msg[2]
      @stderr.write 'error:parse_error\n'
      return

    unless @webSocket
      @stderr.write 'error:client_not_started\n'
      return

    @webSocket.send JSON.stringify
      type: msg[1]
      data: msg[2]
