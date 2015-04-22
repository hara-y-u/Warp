;;; warp-web-server.el --- warp server for web resources

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

(require 'cl-lib)
(require 'warp-util)
(require 'warp-ws-server)
(require 'elnode)
(require 'browse-url)


;;; Main Struct

(cl-defstruct (warp-web-server
            (:constructor nil)
            (:constructor warp-web-server--inner-make))
  (root-directory "~/public_html")
  (port 8800)
  (host "localhost")
  ; only for inner use
  warp-ws-server
  static-handler
  (asset-directory
   (expand-file-name
    "warp-client"
    (file-name-directory (or load-file-name "."))))
  (asset-files
   '("index.html" "client.js" "client.css" "content.html"))
  asset-handler
  )

; Buffer dedicated warp-web-server
(defvar warp-web-server nil)
(make-variable-buffer-local 'warp-web-server)


;;; Constructor

(defun make-warp-web-server (&rest options)
  "Make warp-web-server instance and start serving process.
This function takes optional values for warp-web-server:

  root-directory: root directory for serve files
  port: port number for inner webserver
  host: host name for inner webserver

Returns warp-web-server if succeeded, else returns nil.
"
  (let* ((web-server
          (apply 'warp-web-server--inner-make options))
         (web-server-proc
          (warp-web-server--start-server-proc web-server)))
    (if (null web-server-proc)
        (progn
          (message "warp: faild to start web server for client")
          nil)
      web-server)))


;;; Destructor

(defun delete-warp-web-server (web-server)
  "Delete warp-web-server instance"
  (let ((ret ;; process if success, otherwise nil
         (warp-web-server--stop-server-proc web-server)))
    (if (null ret)
        (progn (message "warp: failed to stop web-server for client.")
               nil)
      ret)))


;;; Properties

(defun warp-web-server-client-url (web-server)
  (let ((host (warp-web-server-host web-server))
        (port (warp-web-server-port web-server))
        (wsport (warp-ws-server-port
                 (warp-web-server-warp-ws-server
                  web-server))))
    (format "http://%s:%d?wsport=%d" host port wsport)))


;;; Methods (double hyphen means private)

(defun warp-web-server--start-server-proc (web-server)
  "Start web server process for web-server and return its process"
  (let ((static-handler (warp-web-server-static-handler web-server))
        (root-directory (warp-web-server-root-directory web-server))
        (asset-handler (warp-web-server-asset-handler web-server))
        (asset-directory (warp-web-server-asset-directory web-server)))
    (fset static-handler
          (elnode-webserver-handler-maker root-directory))
    (fset asset-handler
          (elnode-webserver-handler-maker asset-directory))
    (cdar (elnode-start
           'warp-web-server--handler
           :port (warp-web-server-port web-server)
           :host (warp-web-server-host web-server)))))

(defun warp-web-server--handler (httpcon)
  (let* ((path (elnode-http-pathinfo httpcon))
         (fragment (substring path 1)))
    (when (equal path "/") (setq fragment "index.html"))
    (if (member fragment warp-web-server--assets)
        (funcall (warp-web-server-asset-handler web-server) httpcon)
      (funcall (warp-web-server-static-handler web-server) httpcon))))

(defun warp-web-server--stop-server-proc (web-server)
  "Stop web server proc for web-server.
This function returns process object if succeed, else return nil"
  (let ((ret (elnode-stop
              (warp-web-server-port web-server))))
    (if (null ret)
        nil
      (cdar ret))))

;; TODO: If client gets large spec, cut this out to warp-client object.
(defun warp-web-server-open-client (web-server)
  (browse-url (warp-web-server-client-url web-server)))


(provide 'warp-web-server)

;;; warp-web-server.el ends here
