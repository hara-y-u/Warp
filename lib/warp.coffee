#!/usr/bin/env coffee

http = require 'http'
url = require 'url'
WebSocketServer = new require('websocket').server
messageParser = /([\w\d]+):(.+)/

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
      #closed-screen { display:none; height:100%; width:100%;
                       text-align: center; font-size: 3em;
                       position:absolute; left:0; top:1.2em;
                       background-color:rgba(0,0,0,0.4); z-index: 99999;}
    </style>
    <script src="/client.js"></script>
  </head>
  <body>
    <div id="closed-screen">It seems server has stopped :(</div>
    <header>
      Warp Client #<span id="client-id"/>
    </header>
    <iframe id="warp-frame" src="/content.html"/>
  </body>
</html>
'''

CLIENT_JS = """
(function () {

var soc = new WebSocket('ws://' + location.host + '/', 'warp')
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
      case 'client_id':
        document.getElementById('client-id').innerText = msg.data;
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
  document.getElementById('closed-screen').setAttribute('style', 'display:block;');
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
    @sockets = {}
    @socketId = 0
    process.on 'SIGINT', @onSigint
    process.on 'uncaughtException', (err) =>
      @stderr.write "error:uncaught_exception #{err}\n"

  onSigint: () =>
    @httpServer.close() if @httpServer
    process.exit()

  startServer: () =>
    @startHttpServer()
    @startWebSocketServer()
    @startStdinListener()

  startHttpServer: () =>
    @httpServer = http.createServer @handleHttpRequest
    @httpServer.listen @port
    @stdout.write "start:lotalhost:#{@port}\n"

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

  # WebSocket
  startWebSocketServer: () =>
    @webSocketServer = new WebSocketServer
      httpServer: @httpServer

    @webSocketServer.on 'request', (req) =>
      webSocket = req.accept 'warp', req.origin

      # Make internal reference for client id
      id = @socketId++

      webSocket.send JSON.stringify
        type: 'client_id'
        data: id

      @sockets[id] = webSocket

      #From Client
      webSocket.on 'message', (msg) =>
        msg = JSON.parse(msg.utf8Data);
        @handleWebSocketMessage msg, id

      webSocket.on 'close', () =>
        delete @sockets[id]
        process.stdout.write "client_#{id}_status:closed\n"

  handleWebSocketMessage: (msg, id) =>
    process.stdout.write "client_#{id}_#{msg.type}:#{msg.data}\n"

  sendWebSocketMessage: (msg, id) =>
    if id
      @sockets[id].send (JSON.stringify msg)
    else
      for id, socket of @sockets
        socket.send (JSON.stringify msg)

  # STDIN
  startStdinListener: () =>
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

    @sendWebSocketMessage type: msg[1], data: msg[2]
