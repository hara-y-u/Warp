(function() {
  var PORT, Warp, WebSocketServer, http, url,
    __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };

  http = require('http');

  url = require('url');

  WebSocketServer = new require('websocket').server;

  PORT = 8898;

  module.exports = Warp = (function() {

    function Warp(options) {
      if (options == null) options = {};
      this.handleStdinEof = __bind(this.handleStdinEof, this);
      this.handleStdin = __bind(this.handleStdin, this);
      this.startStdinListener = __bind(this.startStdinListener, this);
      this.sendWebSocketMessage = __bind(this.sendWebSocketMessage, this);
      this.handleWebSocketMessage = __bind(this.handleWebSocketMessage, this);
      this.startWebSocketServer = __bind(this.startWebSocketServer, this);
      this.handleHttpRequest = __bind(this.handleHttpRequest, this);
      this.startHttpServer = __bind(this.startHttpServer, this);
      this.startServer = __bind(this.startServer, this);
      this.onSigint = __bind(this.onSigint, this);
      this.contentHtml = __bind(this.contentHtml, this);
      this.clientJs = __bind(this.clientJs, this);
      this.clientHtml = __bind(this.clientHtml, this);
      this.autoCloseClients = options.autoCloseClients;
      this.port = options.port || PORT;
      this.stdin = process.stdin;
      this.sockets = {};
      this.socketId = 0;
      this.buf = [];
      this.lastHtml = '';
      this.isReceivingText = false;
      process.on('SIGINT', this.onSigint);
    }

    Warp.prototype.clientHtml = function() {
      return '<!DOCTYPE html>\n<html>\n  <head>\n    <title>Warp</title>\n    <style>\n      * { margin:0; padding:0 }\n      html {height:100%; overflow:hidden;}\n      header { display:none; height:1.2em; overflow:hidden; border-bottom:solid 1px #bbb; }\n      body { height:100%; width:100%; }\n      iframe#warp-frame { height:100%; width:100%; border:0; }\n      #closed-screen { display:none; height:100%; width:100%;\n                       text-align: center; font-size: 3em; color: #fff;\n                       position:absolute; left:0; top:0;\n                       background-color:rgba(0,0,0,0.8); z-index: 99999;\n                       padding: 2em;\n                      }\n    </style>\n    <script src="/client.js"></script>\n  </head>\n  <body>\n    <div id="closed-screen">Server not running :(</div>\n    <header>\n      Warp Client #<span id="client-id"/>\n    </header>\n    <iframe id="warp-frame" src="/content.html"/>\n  </body>\n</html>';
    };

    Warp.prototype.clientJs = function() {
      return "(function () {\n\nvar soc = new WebSocket('ws://' + location.host + '/', 'warp')\n, nop = function(){}\n, startupStack = []\n;\n\nstartupStack.push(function() {\n  soc.send(JSON.stringify({ type:'status', data:'start' }));\n\n  var frame = document.getElementById('warp-frame');\n\n  soc.onmessage = function(msg) {\n    msg = JSON.parse(msg.data);\n    console.log(msg.type, msg.data);\n    switch (msg.type) {\n      // case 'reload':\n      //   frame.contentWindow.location.reload();\n      //   break;\n      case 'load':\n      case 'url':\n        frame.contentWindow.location.href = msg.data;\n        break;\n      case 'html':\n        frame.contentDocument.documentElement.innerHTML = msg.data;\n        document.title = frame.contentDocument.title\n          //.replace(/<!doctype[^>]*>/i, '').replace(/<\\/?html[^>]*>/i, '');\n        break;\n      case 'client_id':\n        document.getElementById('client-id').innerText = msg.data;\n        break;\n      default:\n        soc.send(JSON.stringify({ type:'error', data:'unknown_type' }));\n    }\n  };\n\n});\n\nstartupStack.push(nop);\nsoc.onopen = function() { startupStack.pop()(); };\n\nstartupStack.push(nop);\ndocument.addEventListener('DOMContentLoaded', function() { startupStack.pop()(); });\n\nstartupStack.pop()();\n\nsoc.onclose = function() {\n  if(" + this.autoCloseClients + ") { window.open('', '_self', ''); window.close(); }\n  document.getElementById('closed-screen').setAttribute('style', 'display:block;');\n};\n\n}());";
    };

    Warp.prototype.contentHtml = function() {
      return '<html>\n  <body>\n  </body>\n</html>';
    };

    Warp.prototype.onSigint = function() {
      if (this.httpServer) this.httpServer.close();
      return process.exit();
    };

    Warp.prototype.startServer = function() {
      this.startHttpServer();
      this.startWebSocketServer();
      return this.startStdinListener();
    };

    Warp.prototype.startHttpServer = function() {
      this.httpServer = http.createServer(this.handleHttpRequest);
      this.httpServer.listen(this.port);
      return console.log("start:lotalhost:" + this.port);
    };

    Warp.prototype.handleHttpRequest = function(req, res) {
      switch (url.parse(req.url).path) {
        case '/':
          res.writeHead(200, {
            'Content-Type': 'text/html'
          });
          res.write(this.clientHtml(), 'utf-8');
          break;
        case '/content.html':
          res.writeHead(200, {
            'Content-Type': 'text/html'
          });
          res.write(this.contentHtml(), 'utf-8');
          break;
        case '/client.js':
          res.writeHead(200, {
            'Content-Type': 'text/javascript'
          });
          res.write(this.clientJs(), 'utf-8');
          break;
        default:
          res.writeHead(404, {
            'Content-Type': 'text/plain'
          });
          res.write('404 Not Found\n');
      }
      return res.end();
    };

    Warp.prototype.startWebSocketServer = function() {
      var _this = this;
      this.webSocketServer = new WebSocketServer({
        httpServer: this.httpServer
      });
      return this.webSocketServer.on('request', function(req) {
        var id, webSocket;
        webSocket = req.accept('warp', req.origin);
        id = _this.socketId++;
        webSocket.send(JSON.stringify({
          type: 'client_id',
          data: id
        }));
        _this.sockets[id] = webSocket;
        webSocket.on('message', function(msg) {
          msg = JSON.parse(msg.utf8Data);
          return _this.handleWebSocketMessage(msg, id);
        });
        return webSocket.on('close', function() {
          delete _this.sockets[id];
          return console.log("client_" + id + "_status:closed");
        });
      });
    };

    Warp.prototype.handleWebSocketMessage = function(msg, id) {
      console.log("client_" + id + "_" + msg.type + ":" + msg.data);
      if (msg.type === 'status') {
        if (msg.data === 'start') {
          return this.sendWebSocketMessage({
            type: 'html',
            data: this.lastHtml
          });
        }
      }
    };

    Warp.prototype.sendWebSocketMessage = function(msg, id) {
      var socket, _ref, _results;
      if (id) {
        return this.sockets[id].send(JSON.stringify(msg));
      } else {
        _ref = this.sockets;
        _results = [];
        for (id in _ref) {
          socket = _ref[id];
          _results.push(socket.send(JSON.stringify(msg)));
        }
        return _results;
      }
    };

    Warp.prototype.startStdinListener = function() {
      this.stdin.resume();
      this.stdin.setEncoding('utf8');
      this.stdin.on('data', this.handleStdin);
      return this.stdin.on('end', this.handleStdinEof);
    };

    Warp.prototype.handleStdin = function(chunk) {
      var char, tmp, _i, _len, _results;
      _results = [];
      for (_i = 0, _len = chunk.length; _i < _len; _i++) {
        char = chunk[_i];
        if (char === "\x01") {
          this.isReceivingText = true;
          continue;
        } else if (char === "\x02") {
          this.isReceivingText = false;
          tmp = this.buf.join('');
          if (/\S+/.test(tmp)) {
            this.lastHtml = tmp;
            this.sendWebSocketMessage({
              type: 'html',
              data: this.lastHtml
            });
          }
          this.buf = [];
          continue;
        }
        if (this.isReceivingText) this.buf.push(char);
        _results.push(false);
      }
      return _results;
    };

    Warp.prototype.handleStdinEof = function() {};

    return Warp;

  })();

}).call(this);
