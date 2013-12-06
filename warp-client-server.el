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

(eval-when-compile (require 'cl))
(require 'warp-util)
(require 'warp-ws-server)
(require 'elnode)
(require 'browse-url)


;;; Main Struct

(defstruct (warp-client-server
            (:constructor nil)
            (:constructor warp-client-server--inner-make))
  (root-directory default-directory)
  (port 8800)
  (host "localhost")
  ; only for inner use
  (assets-directory
   (expand-file-name
    "warp-client"
    (file-name-directory (or load-file-name "."))))
  warp-ws-server
  web-server-proc
  )


;;; Constructor
  
(defun make-warp-client-server (&rest options)
  "Make warp-client-server instance and start serving process.
This function takes optional values for warp-client-server:
  root-directory: root directory for serve files
  port: port number for inner webserver
  host: host name for inner webserver
"
  (let* ((client-server
          (apply 'warp-client-server--inner-make options))
         (web-server-proc
          (warp-client-server--start-web-server client-server)))
    (if (null web-server-proc)
        (progn
          (message "warp: faild to start web server for client")
          nil)
      (progn
        (process-put web-server-proc
                     :warp-client-server
                     client-server)
        (setf (warp-client-server-web-server-proc client-server)
              web-server-proc)
        client-server))))


;;; Destructor
  
(defun delete-warp-client-server (client-server)
  "Delete warp-client-server instance"
  (let ((ret ;; process if success, otherwise nil
         (warp-client-server--stop-web-server client-server)))
    (if (null ret)
        (message "warp: failed to stop web-server for client.")
      (progn (setf (warp-client-server-web-server-proc client-server)
                   nil)
             ret))))


;;; Properties

(defun warp-client-server-url (client-server)
  (let ((host (warp-client-server-host client-server))
        (port (warp-client-server-port client-server))
        (wsport (warp-ws-server-port
                 (warp-client-server-warp-ws-server
                  client-server))))
    (format "http://%s:%d?wsport=%d" host port wsport)))


;;; Methods (double hyphen means private)

(defun warp-client-server--start-web-server (client-server)
  "Start web server for client-server and return its process"
  (cdar (elnode-start
         'warp-client-server--web-server-handler
         :port (warp-client-server-port client-server)
         :host (warp-client-server-host client-server))))

(defun warp-client-server--web-server-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon "<html><b>HELLO!</b></html>"))

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
