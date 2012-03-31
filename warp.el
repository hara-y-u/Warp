;;;warp.el --- Web Applications Realtime Preview Minor Mode
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2012 by yukihr

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

;;;Version: 0.0.1
;; Author: yukihr (Yukihiro Hara), <yukihr [at] gmail [dot] com>
;; URL: http://yukihr.github.com/

;;;Contributers

;;;Installation
;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;; (require 'warp)
;;
;; If you use default key map, Put the following expression into your ~/.emacs.
;;
;; ;;; (warp-set-default-keymap)

;;; Commentary:

;;; Commands:

;;; TODO
;;  2012-03-31 Dont show precess output (?) on buffer

;;; Code:

(require 'easy-mmode)

;; Customize
(defgroup warp nil
  "Web Application Realtime Preview mode"
  :group 'convenience
  :prefix "warp-")

(defcustom warp-server-port-base 8800
  "Base port number for warp server"
  :type 'integer
  :group 'warp)

(defcustom warp-auto-open-client t
  "Open client in browser when warp-mode turned on"
  :type 'boolean
  :group 'warp)

(defcustom warp-auto-close-client t
  "Close client when warp-mode turned off"
  :type 'boolean
  :group 'warp)

(defcustom warp-idle-time 0.3
  "Time for idle detection on html sending mode"
  :type 'float
  :group 'warp)

(defcustom warp-server-command "warp"
  "Warp server command"
  :type 'string
  :group 'warp)

(defcustom warp-server-command-args nil
  "Arguments for warp server command"
  :type 'list
  :group 'warp)

(define-minor-mode warp-mode
  "Web Application Realtime Preview minor mode"
  :lighter " Warp"
  :group  'warp
  (if warp-mode
      (progn (warp-start-server)
             (if warp-auto-open-client
                 (progn (sleep-for 3)
                        (warp-open-client)))
             (run-with-idle-timer warp-idle-time t
                                  'warp-html-send-current-buffer))
    (warp-interrupt-server)))


;; User Command
(defun warp-start-server ()
  "Start warp server for current buffer"
  (interactive)
  (progn (set (make-local-variable 'warp-server-port)
              (warp-get-server-port))
         (set (make-local-variable 'warp-server-process)
              (apply 'warp-start-server-internal (current-buffer)
                     "-p" (number-to-string warp-server-port)
                     (append warp-server-command-args
                             (if warp-auto-close-client '("-c") nil))))))

(defun warp-interrupt-server ()
  "Send SIGINT to warp server"
  (interactive)
  (if (processp warp-server-process)
      (interrupt-process warp-server-process)))

(defun warp-send-server-string (string)
  "Send string to warp server's STDIN"
  (interactive "sString send to warp: ")
  (if (processp warp-server-process)
      (process-send-string warp-server-process string)))

(defun warp-html-command (string)
  "Send string as html command data to warp server's STDIN"
  (interactive "sHTML string send to warp: ")
  (warp-send-server-string (concat "html:" string "\n")))

(defun warp-html-send-current-buffer ()
  "Send warp server current buffer content as HTML data"
  (interactive)
  (warp-html-command (replace-regexp-in-string "\n" "" (buffer-string))))

(defun warp-open-client ()
  "Open warp client within default browser"
  (interactive)
  (browse-url (concat "http://localhost:" (number-to-string warp-server-port) "/")))


;; Fundamental
(defvar warp-server-command-path
  (file-name-directory (or load-file-name "."))
  "Directory of warp server command")

(defvar warp-current-server-port
  warp-server-port-base
  "Current port number for server.
Be sure to get port number through warp-get-server-port.")

(defun warp-get-server-port ()
  "Get next port number for server."
  (setq warp-current-server-port (1+ warp-current-server-port)))

(defun warp-receive-server-output (process output)
  "Receive warp server outputs"
  (message "Warp: %s" output))

(defun warp-start-server-internal (buffer &rest args)
  "Start warp server and returns server process.
This function takes buffer to which bind server process to.
Pass nil as buffer if you wish no buffer to be bound."
  (let ((process
         (apply 'start-process "warp-server" buffer
                (expand-file-name warp-server-command warp-server-command-path) args)))
    (set-process-filter process 'warp-receive-server-output) process))


;; Provide
(provide 'warp)