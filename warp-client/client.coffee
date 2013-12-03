# Utils
$id = ()->
  document.getElementById.apply(document, arguments)

$cls = ()->
  document.getElementsByClassName.apply(document, arguments)

parseUrl = (url) ->
  # From http://snipplr.com/view.php?codeview&id=12659
  a = document.createElement('a')
  a.href = url
  {
    source: url
    protocol: a.protocol.replace ':',''
    host: a.hostname
    port: a.port
    search: a.search
    params: ( ()->
      ret = {}
      segs = a.search.replace(/^\?/,'').split('&')
      for seg in segs
        s = seg.split('=')
        ret[s[0]] = s[1]
      ret )()
    file: (a.pathname.match(/\/([^\/?#]+)$/i) || ['',''])[1]
    hash: a.hash.replace('#','')
    path: a.pathname.replace(/^([^\/])/,'/$1')
    relative: (a.href.match(/tp:\/\/[^\/]+(.+)/) || ['',''])[1]
    segments: a.pathname.replace(/^\//,'').split('/')
  }

class Deferred
  nop = (->)
  constructor: ->
    @_stack = []
  wait: (fn) ->
    @_stack.push nop
    fn @_done
    @
  _done: =>
    @_stack.pop()()
  then: (fn) ->
    @_stack.unshift fn
    @_stack.pop()()
    @

class WarpMessage
  constructor: (json)->
    @type = json.type or 'notify'
    @name = json.name or 'default'
    @data = json.data or {}
  encode: ->
    JSON.stringify
      type: @type
      name: @name
      data: @data

# Start up
wsport = parseUrl(location.href).params.wsport
socket = new WebSocket('ws://localhost:' + wsport + '/', "warp")
frame = null
dfd = new Deferred

dfd
  .wait (done)->
    socket.onopen = ->
      done()
  .wait (done)->
    document.addEventListener "DOMContentLoaded", ->
      # On Firefox, have to wait loading of iframe,
      # because doc will have reference to empty content before load.
      frame = $id("warp-frame")
      frame.onload = ->
        done()
        frame.onload = (->)
  .then ->
    socket.onmessage = (message)->
      switch(message.type)
        when 'command'
          handleCommand message, frame
        when 'notify'
          handleNotify message
        else
          console.error "Unknown type \"#{message.type}\" on message: #{message}"
    socket.send new WarpMessage(
      type: 'notify'
      name: 'start'
    ).encode()

socket.onclose = ->
  #if(#{@autoCloseClients}) { window.open('', '_self', ''); window.close(); }
  $id("closed-screen").setAttribute "style", "display:block;"

# Handlers
handleCommand = (command, frame) ->
  console.log command.name
  fdoc = frame.contentDocument
  switch command.name
    when "reload"
      frame.src = frame.src
    when "load", "url"
      frame.src = command.args
      $id("loaded-url").value = command.args
    when "renderHtml"
      console.log "renderHtml"
      fdoc.documentElement.innerHTML = command.args
      document.title = frame.contentDocument.title
    when "scroll"
      point = command.data.split(" ")
      inTop = parseInt(point[0], 10)
      inOffset = parseInt(point[1], 10)
      inScreen = parseInt(point[2], 10)
      docHeight = fdoc.documentElement.scrollHeight or fdoc.body.scrollHeight
      screen = fdoc.documentElement.clientHeight / docHeight * 100
      top = (fdoc.documentElement.scrollTop or fdoc.body.scrollTop) / docHeight * 100
      screenDelta = inScreen - screen

      # = Length to Window Top
      # Positive when browser screen is narrow than editor
      # = Hidden Screen Height
      scrollTo = (inTop * docHeight / 100) + ((if screenDelta >= 0 then screenDelta else 0)) * docHeight / 100 * inOffset / 100
      frame.contentWindow.scrollTo 0, scrollTo
    else
      console.error "Unknown command \"#{command.name}\" on command: #{command}"

handleNotify = (notify) ->
  switch notify.name
    when "clientId"
      $id("client-id").textContent = notify.data
    when "test"
      false
    else
      console.error "Unknown notify \"#{notify.name}\" on notify: #{notify}"
