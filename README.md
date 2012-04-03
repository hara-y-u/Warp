# Warp

Warp provides Emacs with ability of realtime preview of html documents and documents which can be converted to htmls.

## Usage

In the document's buffer you want to see preview, execute `warp-mode` command (which is also binded to XXX).
By default, browser window will open and preview of buffer will be displayed in real time.
After finish editing, execute `warp-mode` again to disable preview or simply kill buffer.

## Dependency

- `node.js` 6.11 or later and websocket npm module to have Warp server run.
- Emacs 23 to execute frontend (warp.el).

## Install

1. Install `node.js` according to [this documentation](https://github.com/joyent/node/wiki/Installing-Node.js-via-package-manager) or by [nave](https://github.com/isaacs/nave).

2. Clone this repository.

        $ cd /path/to/your/repo
        $ git clone https://github.com/yukihr/warp.git

3. Install `websocket` npm module.

        $ cd warp
        $ npm install

    or, for grobal install, execute `npm install -g websocket`
  
4. Put following lines into your ~/.emacs.d/init.el or other init file.

        (add-to-list 'load-path "/path/to/warp-directory")
        (require 'warp)
        ;; Belows are needed if you installed websocket npm module globally.
        (setenv "NODE\_PATH" "/path/to/global/node_modules")
        ;; or, if you have set up NODE_PATH in the shell
        (setenv "NODE\_PATH"
          (replace-regexp-in-string
            "\n+$" "" (shell-command-to-string "echo $NODE_PATH")))

## Feature

## Customize
