<!-- Local CSS From: https://github.com/clownfart/Markdown-CSS -->
<link href="readme/markdown.css" rel="stylesheet"></link>

# Warp

Warp provides Emacs with ability of realtime preview of html documents and documents which can be converted to htmls.


## Features

- Realtime HTML Preview using WebSocket
- Auto Open/Close Browser Preview Window
- Auto Scroll Synchronizing
- Supports a Number of Formats


## Confirmed Environment

- Node.js 0.6.11, 0.7.2
- Emacs 23.3, 24.1
- Latest Google Chrome, Mozilla Firefox
- OSX 10.7, Ubuntu Linux 11.04


## Install

1. Install `node.js` with [package](https://github.com/joyent/node/wiki/Installing-Node.js-via-package-manager) or [nave](https://github.com/isaacs/nave).

2. Install [npm](http://npmjs.org/) .

3. Clone this repository.

        $ cd /path/to/your/repos
        $ git clone https://github.com/yukihr/Warp.git 

4. Install `websocket` npm module.

        $ cd Warp
        $ npm install

    or, for grobal install, execute `npm install -g websocket`
  
5. Put following lines into your ~/.emacs.d/init.el or other init file.

        (add-to-list 'load-path "/path/to/warp-directory")
        (require 'warp)
        (global-set-key (kbd "C-c C-w C-w") warp-mode) ;; Modify key bind as you want.

        ;; Set markdown converter (if you want)
        (add-to-list 'warp-format-converter-alist
                     '("\\.md\\|\\.markdown" t (lambda ()
                                                 ;; Set command you are using
                                                 '("markdown"))))

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
                                                 
    - Use Major Mode symbol

            (add-to-list 'warp-format-converter-alist
                         '(markdown-mode t (lambda ()
                                                     '("markdown"))))

    - [textile](http://redcloth.org/textile)

            (add-to-list 'warp-format-converter-alist
                         '("\\.textile" t (lambda () '("redcloth"))))

    - [github-markup](https://github.com/github/markup) (Not tested all formats..)

            (add-to-list 'warp-format-converter-alist
                         '("\\.md\\|\\.markdown\\|\\.textile\\|\\.rdoc\\|\\.org\\|\\.creole\\|\\.mediawiki\\|\\.rst\\|\\.asciidoc\\|\\.pod"
                           nil
                           (lambda ()
                             (let* ((string (buffer-string))
                                    (ext (file-name-extension (buffer-file-name)))
                                    (temp-file (concat "warp-temp." ext)))
                               (with-temp-file temp-file
                                 (insert string))
                               (list "github-markup" temp-file)))))
    

## Known Issues

- Stuck when editing too large buffer.
- Client's display will chatter when resources are fetched from server not browser cache.


## Extra Stuff

### warp-reload.el

`warp-reload.el` provides global minor mode to load specific url on save buffers.

#### Setup

Put following lines to your ~/.emacs.d/init.el

        ;; ;; if you haven't set up yet
        ;; (add-to-list 'load-path "/path/to/warp-directory")

        (require 'warp-reload)
        (global-set-key (kbd "C-c C-w C-r") 'warp-reload)
        
#### Usage

Exec `M-x warp-reload` or `C-c C-w C-r` and input url you want to reload on prompt.
When you want to change url for reloading, exec `M-x warp-reload-change-url` or `C-c C-w C-c`.

## History

- 0.0.2 Availability for Setting Converters with Major Mode Symbol
- 0.0.1 Initial Version
