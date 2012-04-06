<!-- CSS From: https://github.com/clownfart/Markdown-CSS -->
<link href="markdown.css" rel="stylesheet"></link>

# Warp

<!-- Load from local. -->
![Warp](warp.gif)

Warp provides Emacs with ability of realtime preview of html documents and documents which can be converted to htmls.


## Features

- Realtime HTML Preview using WebSocket
- Auto Open/Close Browser Preview Window
- Auto Scroll Synchronizing
- Supports a Number of Formats


## Confirmed Environment

- Node.js 0.6.11/0.7.2
- Emacs 23.3
- Latest Google Chrome


## Install

1. Install `node.js` with [package](https://github.com/joyent/node/wiki/Installing-Node.js-via-package-manager) or [nave](https://github.com/isaacs/nave).

2. Clone this repository.

        $ cd /path/to/your/repos
        $ git clone https://github.com/yukihr/warp.git

3. Install `websocket` npm module.

        $ cd warp
        $ npm install

    or, for grobal install, execute `npm install -g websocket`
  
4. Put following lines into your ~/.emacs.d/init.el or other init file.

        (add-to-list 'load-path "/path/to/warp-directory")
        (require 'warp)
        (global-set-key (kbd "C-c C-w C-w") warp-mode) ;; Modify key bind as you want.
        ;; Below line is needed if you installed websocket npm module globally.
        (setenv "NODE_PATH" "/path/to/global/node_modules")
        ;; or, if you have setup NODE_PATH in the shell
        (setenv "NODE_PATH"
                (replace-regexp-in-string
                 "\n+$" "" (shell-command-to-string "echo $NODE_PATH")))

### Confirmation

1. Restart Emacs once you have installed Warp.

2. Install [Markdown](http://daringfireball.net/projects/markdown/) command line tool into directory in the PATH.

3. Open this README.

        C-x C-f /path/to/warp-directory/README.md

4. Execute warp-mode command.

        M-x warp-mode

   or

        C-c C-w C-w


## Usage

In the document's buffer you want to see preview, execute `warp-mode` command.
By default, a browser tab (or window if you don't have any) will open and html-rendered preview of the buffer will be displayed in real time.
After finished editing, execute `warp-mode` again or simply kill buffer to disable preview.

## Commands

Below commands are bound to specific key bind by default.

- warp-start-server

  Start Warp server. Bound to `C-c C-w s`.

- warp-stop-server

  Stop Warp server. Bound to `C-c C-w i`.

- warp-open-client

  Open Warp client. Bound to `C-c C-w o`.

- warp-send-current-buffer

  Send current buffer to Warp server, convert buffer if needed. Bound to `C-c C-w w`.
  

For reference of other commands, please take a look inside `warp.el`.


## Customize

Exec `M-x customize-group RET warp` to see all customize options.


### init.el Examples

- Disable auto refresh and scroll syncing.

        (custom-set-variables
         '(warp-auto-start-sending nil)
         '(warp-auto-start-auto-scroll nil))
         
- Set custom converter.

        (add-to-list 'warp-format-converter-alist
                     '("\\.textile" t (lambda () '("redcloth"))))
    

## Known Issues

- Stuck when editing too large buffer.
- Client's display will be chattering when resources are fetched from server not browser cache.
