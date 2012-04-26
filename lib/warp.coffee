#!/usr/bin/env coffee

http = require 'http'
url = require 'url'
path = require 'path'
fs = require 'fs'
WebSocketServer = new require('websocket').server

PORT = 8898

module.exports = class Warp
  constructor: (options = {}) ->
    @autoCloseClients = options.autoCloseClients
    @showHeader = options.showHeader
    @enableClientLog = options.enableClientLog
    @port   = options.port or PORT
    @stdin  = process.stdin
    @sockets = {}
    @socketId = 0
    @buf = []
    @lastCommand = {}
    process.on 'SIGINT', @onSigint

  # Static
  clientHtml: () => '''
<!DOCTYPE html>
<html>
  <head>
    <title>Warp</title>
    <link rel="stylesheet" href="/client.css"/>
    <script>
      var $id = function() { return document.getElementById.apply(document, arguments); }
      , $cls = function() { return document.getElementsByClassName.apply(document, arguments); }
      , loadUrl = function() { $id(\'warp-frame\').src = $id(\'loaded-url\').value; }
      , loadUrlOnNewTab = function() { window.open($id(\'loaded-url\').value,\'_blank\'); }
      ;
    </script>
    <script src="/client.js"></script>
  </head>
  <body>
    <div id="closed-screen">Server not running :(</div>
    <header>
      <ul>
        <li>Warp Client #<span id="client-id"/></li>
        <li>
          <input type="text" id="loaded-url" value="/content.html" onchange="loadUrl()"/>
          <a href="#" onclick="loadUrl()">load</a> /
          <a href="#" onclick="loadUrlOnNewTab()">new tab</a>
        </li>
      </ul>
    </header>
    <iframe id="warp-frame" name="warp-frame" src="/content.html"/>
  </body>
</html>
'''

  clientCss: () => """
* { margin:0; padding:0; }
html { height:100%; overflow:hidden; }
header { #{ if @showHeader then '' else 'display:none;' }
         position:fixed; width:100%;
         overflow:hidden; border-bottom:solid 1px #bbb; }
header > ul > li { display: inline; margin: 1rem; }
header > ul > li > input { width: 40%; }
body { height:100%; width:100%; }
iframe#warp-frame { height:100%; width:100%; border:0; #{ if @showHeader then 'margin-top:1.3rem' else '' }}
#closed-screen { display:none; height:100%; width:100%;
                 text-align: center; font-size: 3em; color: #fff;
                 position:absolute; left:0; top:0;
                 background-color:rgba(0,0,0,0.8); z-index: 99999;
                 padding: 2em;
                }
"""

  clientJs: () => """
(function () {

var soc = new WebSocket('ws://' + location.host + '/', 'warp')
, nop = function(){}
, startupStack = []
, frame, doc
;

startupStack.push(function() {
  var scrollTo, point, inTop, inOffset, screen, docHeight
  , top, screenDelta, scrollTo
  ;

  doc = frame.contentDocument;

  soc.onmessage = function(msg) {
    msg = JSON.parse(msg.data);
    #{ if @enableClientLog then 'console.log(msg.type, msg.data);' else '' }
    switch (msg.type) {
      case 'reload':
        frame.src = frame.src;
        break;
      case 'load':
      case 'url':
        frame.src = msg.data;
        $id('loaded-url').value = msg.data;
        break;
      case 'html':
        // // Remember Scroll Position
        // scrollTo = doc.documentElement.scrollTop || doc.body.scrollTop;
        doc.documentElement.innerHTML = msg.data;
        document.title = frame.contentDocument.title;
        // frame.contentWindow.scrollTo(0, scrollTo);
        break;
      case 'scroll':
        point = msg.data.split(' ')
        , inTop = parseInt(point[0], 10)
        , inOffset = parseInt(point[1], 10)
        , inScreen = parseInt(point[2], 10)
        , docHeight = doc.documentElement.scrollHeight || doc.body.scrollHeight
        , screen = doc.documentElement.clientHeight / docHeight * 100
        , top = (doc.documentElement.scrollTop || doc.body.scrollTop) / docHeight * 100
        , screenDelta = inScreen - screen
        ;
        scrollTo = (inTop * docHeight / 100)               // = Length to Window Top
                   + (screenDelta >= 0 ? screenDelta : 0)  // Positive when browser screen is narrow than editor
                   * docHeight / 100                       // = Hidden Screen Height
                   * inOffset / 100;
        frame.contentWindow.scrollTo(0, scrollTo);
        break;
      case 'client_id':
        $id('client-id').textContent = msg.data;
        break;
      default:
        soc.send(JSON.stringify({ type:'error', data:'unknown_type' }));
    }
  };

  soc.send(JSON.stringify({ type:'status', data:'start' }));
});

startupStack.push(nop);
soc.onopen = function() { startupStack.pop()(); };

startupStack.push(nop);
document.addEventListener('DOMContentLoaded', function() {
  // On Firefox, have to wait loading of iframe,
  // because doc will have reference to empty content before load.
  frame = $id('warp-frame');
  frame.onload = function() {
    startupStack.pop()();
    frame.onload = nop;
  }
});

startupStack.pop()();

soc.onclose = function() {
  if(#{@autoCloseClients}) { window.open('', '_self', ''); window.close(); }
  $id('closed-screen').setAttribute('style', 'display:block;');
};

}());
"""

  contentHtml: () => '''
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta charset="UTF-8"/>
    <title></title>
  </head>
  <body>
  </body>
</html>
'''

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
    console.log "start:lotalhost:#{@port}"

  handleHttpRequest: (req, res) =>
    switch url.parse(req.url).path
      when '/'
        res.writeHead 200, 'Content-Type': 'text/html'
        res.write @clientHtml(), 'utf-8'
        res.end()
      when '/content.html'
        res.writeHead 200, 'Content-Type': 'text/html'
        res.write @contentHtml(), 'utf-8'
        res.end()
      when '/client.js'
        res.writeHead 200, 'Content-Type': 'text/javascript'
        res.write @clientJs(), 'utf-8'
        res.end()
      when '/client.css'
        res.writeHead 200, 'Content-Type': 'text/css'
        res.write @clientCss(), 'utf-8'
        res.end()
      else
        @sendStaticFiles req, res

  sendStaticFiles: (req, res) =>
    p = path.join process.cwd(), url.parse(req.url).path
    ext = path.extname p

    _exists = fs.exists or path.exists

    _exists p, (exists) =>
      unless exists
        res.writeHead 404, 'Content-Type': 'text/plain'
        res.write '404 Not Found\n'
        res.end()
        return

      # Suppress Chattering Display
      res.setHeader "Cache-Control", "max-age=100"

      fs.readFile p, 'binary', (err, file) =>
        if err
          res.writeHead 500, 'Content-Type': 'text/plain'
          res.write err + "\n"
          res.end()
          return

        switch ext.substr 1
          when 'png'
            res.writeHead 200, 'Content-Type': 'image/png'
          when 'gif'
            res.writeHead 200, 'Content-Type': 'image/gif'
          when 'jpg', 'jpeg'
            res.writeHead 200, 'Content-Type': 'image/jpeg'
          when 'html', 'htm'
            res.writeHead 200, 'Content-Type': 'text/html'
          when 'js'
            res.writeHead 200, 'Content-Type': 'text/javascript'
          when 'css'
            res.writeHead 200, 'Content-Type': 'text/css'
          when 'swf', 'swfl'
            res.writeHead 200, 'Content-Type': 'application/x-shockwave-flash'
          else
            res.writeHead 200, 'Content-Type': 'text/plain'

        res.write file, 'binary'
        res.end()

  # WebSocket
  startWebSocketServer: () =>
    @webSocketServer = new WebSocketServer
      httpServer: @httpServer

    @webSocketServer.on 'request', (req) =>
      webSocket = req.accept 'warp', req.origin

      # Make internal reference for client id
      id = @socketId++

      @sockets[id] = webSocket

      #From Client
      webSocket.on 'message', (msg) =>
        msg = JSON.parse(msg.utf8Data);
        @handleWebSocketMessage msg, id

      webSocket.on 'close', () =>
        delete @sockets[id]
        console.log "client_#{id}_status:closed"

  handleWebSocketMessage: (msg, id) =>
    console.log "client_#{id}_#{msg.type}:#{msg.data}"
    if msg.type is 'status'
      if msg.data is 'start'
        @sendWebSocketMessage
          type: 'client_id'
          data: id
        switch @lastCommand.type
          when 'load', 'url', 'html'
            @sendWebSocketMessage @lastCommand

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
    @stdin.on 'end', @handleStdinEof

  handleStdin: (chunk) =>
    for char in chunk
      if char is "\x00" # Separate Command On Null
        @handleCommand @buf.join('')
        @buf = []
      else
        @buf.push(char)

    false

  handleCommand: (command) =>
    #console.log command
    command = command.replace /^\n+/, '' # normalize
    try
      if command[0] is "\x1B" # special command
        type = if (m = command.match /^\x1B(\S+)\x1D/) then m[1] else null
        data = if (m = command.match /\x1D([\w \S]+)/) then m[1] else null
        if type is 'load' or type is 'url'
          @lastCommand = type: type, data: data
        @sendWebSocketMessage type: type, data: data
      else # html command
        if /\S+/.test command
          @lastCommand = type: 'html', data: command
          @sendWebSocketMessage @lastCommand
        else
          console.log "Blank HTML Data."
    catch e
      console.log e

  handleStdinEof: () =>
