;;; warp-client-server.el --- warp server for client html

;; Copyright (c) 2013  Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; TODO: rename to warp-web-server

(eval-when-compile (require 'cl))
(require 'warp-util)
(require 'warp-ws-server)
(require 'elnode)
(require 'browse-url)


;;; Main Struct

(defstruct (warp-client-server
            (:constructor nil)
            (:constructor warp-client-server--inner-make))
  (root-directory "~/public_html") ; TODO
  (port 8800)
  (host "localhost")
  ; only for inner use
  warp-ws-server
  )


;;; Constructor

(defun make-warp-client-server (&rest options)
  "Make warp-client-server instance and start serving process.
This function takes optional values for warp-client-server:

  root-directory: root directory for serve files
  port: port number for inner webserver
  host: host name for inner webserver

Returns web-server-proc if succeeded, else returns nil.
"
  (let* ((client-server
          (apply 'warp-client-server--inner-make options))
         (web-server-proc
          (warp-client-server--start-web-server client-server)))
    (if (null web-server-proc)
        (progn
          (message "warp: faild to start web server for client")
          nil)
      client-server)))


;;; Destructor
  
(defun delete-warp-client-server (client-server)
  "Delete warp-client-server instance"
  (let ((ret ;; process if success, otherwise nil
         (warp-client-server--stop-web-server client-server)))
    (if (null ret)
        (progn (message "warp: failed to stop web-server for client.")
               nil)
      ret)))


;;; Properties

(defun warp-client-server-url (client-server)
  (let ((host (warp-client-server-host client-server))
        (port (warp-client-server-port client-server))
        (wsport (warp-ws-server-port
                 (warp-client-server-warp-ws-server
                  client-server))))
    (format "http://%s:%d?wsport=%d" host port wsport)))


;;; Methods (double hyphen means private)

;; TODO: Move this var to property, and set from consumer
(defvar warp-client-server--root-directory "~/public_html")
(make-variable-buffer-local 'warp-client-server--root-directory)

(defvar warp-client-server--static-handler nil)
(make-variable-buffer-local 'warp-client-server--static-handler)
  
(defvar warp-client-server--assets-directory
  (expand-file-name
   "warp-client"
   (file-name-directory (or load-file-name "."))))

(defvar warp-client-server--assets
  ;; TODO: concat assets into index.html
  '("index.html" "client.js" "client.css" "content.html"))

(defvar warp-client-server--assets-handler nil)

(defun warp-client-server--start-web-server (client-server)
  "Start web server for client-server and return its process"
  (setq warp-client-server--root-directory default-directory) ; TODO: don't access buffer local var (default-directory)
  (fset 'warp-client-server--static-handler
        (elnode-webserver-handler-maker warp-client-server--root-directory))
  (fset 'warp-client-server--assets-handler
        (elnode-webserver-handler-maker warp-client-server--assets-directory))
  (cdar (elnode-start
         'warp-client-server--web-server-handler
         :port (warp-client-server-port client-server)
         :host (warp-client-server-host client-server))))

(defun warp-client-server--web-server-handler (httpcon)
  (let* ((path (elnode-http-pathinfo httpcon))
         (fragment (substring path 1)))
    (when (equal path "/") (setq fragment "index.html"))
    (if (member fragment warp-client-server--assets)
        (funcall 'warp-client-server--assets-handler httpcon)
      (funcall 'warp-client-server--static-handler httpcon))))

(defun warp-client-server--stop-web-server (client-server)
  "Stop web server for client-server.
This function returns process object if succeed, else return nil"
  (let ((ret (elnode-stop
              (warp-client-server-port client-server))))
    (if (null ret)
        nil
      (cdar ret))))

(provide 'warp-client-server)

;;; warp-client-server.el ends here
