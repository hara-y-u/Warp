;;;warp.el --- Web Documents Realtime Preview Minor Mode
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

;;; Version: 0.0.1
;;  Author: yukihr (Yukihiro Hara), <yukihr [at] gmail [dot] com>
;;  URL: http://yukihr.github.com/

;;; Contributers:
;;; Installation:
;;; Commentary:
;;; Commands:
;;  See README.md

;;; TODO
;;  * Default Keybind
;;  * Option: Set Custom CSS
;;  * Feature for reload-to-check Web Application
;;  * Stuck when large buffer is sent

;;; Code:

;;;;;;;;; Customize ;;;;;;;;;
(defgroup warp nil
  "Warp mode"
  :group 'editing
  :prefix "warp-")

(defcustom warp-server-port-base 8800
  "Base port number for warp server"
  :type 'integer
  :group 'warp)

(defcustom warp-auto-open-client t
  "Open client in browser when emacs send buffer to server, if not opened yet."
  :type 'boolean
  :group 'warp)

(defcustom warp-auto-close-client t
  "Close client when `warp-mode' is turned off.
Clients opened in Firefox can't support this."
  :type 'boolean
  :group 'warp)

(defcustom warp-auto-start-sending t
  "Start sending html to the server when warp-mode is turned on"
  :type 'boolean
  :group 'warp)

(defcustom warp-auto-start-auto-scroll t
  "Start auto-scrolling clients when warp-mode is turned on"
  :type 'boolean
  :group 'warp)

(defcustom warp-idle-time 0.4
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

(defcustom warp-format-converter-alist
  (list
   '("\\.md\\|\\.markdown" t (lambda ()
                              '("markdown")))
   )
  "Alist of converters used for converting specific format to html. The format is:

'\(\(\"Filename or Regexp\" \"Need STDIN Input\" \"Function which returns convert command in list format\"\)..\)

If `warp-mode' is enabled on buffer its file name matches \"Filename or Regexp\",
`warp-mode' converts buffer string to HTML using converter command returned by
associated function before send string to server. If \"Need STDIN Input\" is not nil,
send current buffer string to command's STDIN."
  :type 'list
  :group 'warp)

(defvar warp-mode-hook nil
  "Hook for warp mode")

;;;###autoload
(define-minor-mode warp-mode
  "Warp minor mode"
  :lighter " Warp"
  :group  'warp
  (if warp-mode
      (progn (warp-start-server)
             (run-hooks 'warp-mode-hook))
    (warp-stop-server)))


;;;;;;;;; Fundamental ;;;;;;;;;
; Buffer Local Variables
(defvar warp-server-process nil
  "Warp server process.")
(make-variable-buffer-local 'warp-server-process)

(defvar warp-server-port nil
  "Port for Warp server.")
(make-variable-buffer-local 'warp-server-port)

(defvar warp-auto-opened-client-once nil
  "Indicates if opened client automatically at least once.")
(make-variable-buffer-local 'warp-auto-opened-client-once)

(defvar warp-last-modified-tick nil
  "Save buffer's modified state.")
(make-variable-buffer-local 'warp-last-modified-tick)

(defvar warp-auto-sending-timer nil
  "Save timer for auto sending.")
(make-variable-buffer-local 'warp-auto-sending-timer)

(defvar warp-auto-scroll-timer nil
  "Save timer for auto scroll.")
(make-variable-buffer-local 'warp-auto-scroll-timer)

; Global Variables
(defvar warp-server-command-path
  (file-name-directory (or load-file-name "."))
  "Directory of warp server command")

(defvar warp-current-server-port
  warp-server-port-base
  "Current port number for server.
Be sure to get port number by `warp-get-server-port'.")

(defvar warp-start-server-listeners '()
  "Executed when server start.")

(defvar warp-stop-server-listeners '()
  "Executed when server stop.")

(defmacro warp-add-server-listener (when fn)
  `(let* ((list-sym (intern (concat "warp-" ,when "-server-listeners")))
          (list (symbol-value list-sym)))
     (or (member ',fn list) ; dont add same fn
         (set list-sym (append '(,fn) list)))))

; Basic Funcs
(defun warp-get-server-port ()
  "Get next port number for server."
  (setq warp-current-server-port (1+ warp-current-server-port)))

(defun warp-process-running-p (process)
  "Return if process is running"
  (cond
   ((and (processp process)
        (equal (process-status process) 'run)) t)
   (t nil)))

(defun warp-server-process-filter (process output)
  "Receive warp server outputs"
  (message "Warp Server: %s" (replace-regexp-in-string "\n+$" "" output)))

(defun warp-server-process-sentinel (process event)
  "Sentinel for warp server process."
  (progn
    ; stop listeners
    (when (equal (process-status process) 'exit)
      (dolist (fn warp-stop-server-listeners)
        (funcall fn)))))

(defun warp-start-server-process (buffer &rest args)
  "Start warp server and returns server process.
This function takes buffer to which bind server process to.
Pass nil as buffer if you wish no buffer to be bound."
  (let ((process
         (apply 'start-process "warp-server" buffer
                (expand-file-name warp-server-command warp-server-command-path) args)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process 'warp-server-process-sentinel)
    (set-process-filter process 'warp-server-process-filter)
    ; start listeners
    (let ((exitp
           (catch 'break
             (while (not (equalp (process-status process) 'run))
               (when (equalp (process-status process) 'exit)
                 (throw 'break t))
               (sleep-for 0 10))))) ; wait till start
      (if (not exitp)
        (dolist (fn warp-start-server-listeners)
          (funcall fn))
        (message "Warp: Process abnormally exited.")))
    process))


;;;;;;;;; User Commands ;;;;;;;;;
; Server
(defun warp-server-running-p ()
  (interactive)
  (and (processp warp-server-process)
       (warp-process-running-p warp-server-process)))

(defun warp-start-server ()
  "Start warp server for current buffer"
  (interactive)
  (if (not (warp-server-running-p))
      (setq warp-server-port (warp-get-server-port)
            warp-server-process
            (apply 'warp-start-server-process (current-buffer)
                   "-p" (number-to-string warp-server-port)
                   (append warp-server-command-args
                           (if warp-auto-close-client '("-c") nil))))
    nil))

(defun warp-stop-server ()
  "Stop warp server"
 (interactive)
 (when (warp-server-running-p)
   (interrupt-process warp-server-process)))

; Client
(defun warp-open-client ()
  "Open warp client within default browser"
  (interactive)
  (browse-url (concat "http://localhost:" (number-to-string warp-server-port) "/")))

; Server Command
(defun warp-send-server-string (string)
  "Send string to warp server's STDIN"
  (interactive "sString send to warp: ")
  (if (warp-server-running-p)
      (process-send-string warp-server-process string)
    ;; (progn (process-send-string warp-server-process string)
    ;;        (message "%s" string)) ;; debug
    (message "Warp: Server not running..")))

(defun warp-send-server-eof ()
  "Send EOF to warp server's STDIN."
  (interactive)
  (if (warp-server-running-p)
      (process-send-eof warp-server-process)
    (message "Warp: Server not running..")))

(defun warp-send-command (string)
  "Send string as a command data to warp server's STDIN"
  (interactive "sCommand string send to warp: ")
  (unless (string-equal "" string)
    (warp-send-server-string string)
    (warp-send-server-string " ") ; Send Null to Separate Command
    (warp-send-server-string "\n") ; Determine Command
    ))

; Current Buffer
(defun warp-buffer-string ()
  "Get whole buffer string"
  (save-restriction
    (widen)
    (save-excursion ; need this?
      (buffer-string))))

(defun warp-send-current-buffer-as-html ()
  "Send warp server current buffer content as HTML data"
  (interactive)
  (warp-send-command
   (encode-coding-string (warp-buffer-string) 'utf-8)))

(defun warp-send-current-buffer-converting ()
  "Send warp server current buffer content converting to HTML data."
  (interactive)
  (save-restriction
    (widen)
    (let* (html-message
           (convert-options
            (assoc-default buffer-file-name
                           warp-format-converter-alist 'string-match))
           (need-stdin (car convert-options))
           (convert-command-list (funcall (car (cdr convert-options))))
           (convert-command (car convert-command-list))
           (convert-args (cdr convert-command-list))
           (beg (point-min))
           (end (point-max))
           (warp-server-process warp-server-process)
           (buffer-output (get-buffer-create "*warp-convert*")))
      (with-current-buffer buffer-output (erase-buffer))
      ;(message "%s %s %s %s" beg end need-stdin convert-command)
      (if need-stdin
          (apply 'call-process-region
                 beg end
                 convert-command
                 nil
                 buffer-output
                 nil
                 convert-args)
        (apply 'call-process
               convert-command
               nil
               buffer-output
               nil
               convert-args))
      (with-current-buffer buffer-output
        (setq html-message (buffer-string)))
      (warp-send-command html-message))))

;;;; Async version (won't work well)
;; (defun warp-send-current-buffer-converting ()
;;   "Send warp server current buffer content converting to HTML data."
;;   (interactive)
;;   (progn (warp-send-server-string "")
;;          (let* ((convert-command
;;                  (funcall (car (assoc-default buffer-file-name
;;                                               warp-format-converter-alist 'string-match))))
;;                 (convert-process
;;                  (apply 'start-process "warp-convert" (current-buffer) convert-command)))
;;            (set-process-query-on-exit-flag convert-process nil)
;;            (set-process-filter convert-process
;;                                '(lambda (process output)
;;                                   (warp-send-server-string output)))
;;            (set-process-sentinel convert-process
;;                                  '(lambda (process event)
;;                                     (when (equal (process-status process) 'exit)
;;                                       (warp-send-server-string ""))))
;;            ;; TODO: IF command need stdin
;;            (process-send-string convert-process (concat (warp-buffer-string) "\n"))
;;            (process-send-eof convert-process))
;;          ))

(defun warp-current-buffer-need-convert-p ()
  "See if conversion is needed for current buffer"
  (not (null (assoc-default buffer-file-name
                         warp-format-converter-alist 'string-match))))

(defun warp-send-current-buffer ()
  "Send warp server current buffer. Convert string if setting for current buffer exists"
  (interactive)
  (if (warp-current-buffer-need-convert-p)
      (warp-send-current-buffer-converting)
    (warp-send-current-buffer-as-html)))

; Scroll
(defun warp-get-scroll-point ()
  "Get scroll point for request server scroll.
   Returns list that format is (top offset screen)
   Where,
     top    is {Lines to Window Top}   / {Full Lines}      * 100
     offset is {Lines from Window Top} / {Lines in Window} * 100
     screen is {Lines in Window}       / {Full Lines}      * 100"
  (interactive)
  (let* ((full-lines            (count-lines (point-min) (point-max)))
         (window-lines          (count-lines (window-start) (window-end)))
         (lines-to-window-top   (count-lines (point-min) (window-start)))
         (lines-from-window-top (count-lines (window-start) (point)))
         (top                   (/ (* 100 lines-to-window-top) full-lines))
         (offset                (+ 2 (/ (* 100 lines-from-window-top) window-lines)))
         (screen                (/ (* 100 window-lines) full-lines)))
    (list top offset screen)))

(defun warp-scroll-client-to (point)
  (warp-send-command
   (concat "scroll"
           (mapconcat 'number-to-string point " "))))

(defun warp-scroll-to-current-line ()
  (interactive)
  (warp-scroll-client-to (warp-get-scroll-point)))


;;;;;;;;; Automatic Feature Settings ;;;;;;;;;
; Auto Sending
(defun warp-sending-running-p ()
  (interactive)
  (timerp warp-auto-sending-timer))

(defun warp-start-sending-current-buffer ()
  "Start sending html to the server"
  (interactive)
  (if (not (warp-sending-running-p))
      (progn (setq warp-last-modified-tick -1
                   warp-auto-sending-timer
                   (run-with-idle-timer
                    warp-idle-time
                    t
                    '(lambda ()
                       (when (warp-sending-running-p) ; Works only on called buffer.
                         (when (not (equal warp-last-modified-tick
                                           (buffer-modified-tick)))
                           ; (message "send: %s %s" warp-last-modified-tick (buffer-modified-tick))
                           (set 'warp-last-modified-tick (buffer-modified-tick))
                           (warp-send-current-buffer)))))))
    (message "Warp: Already Sending to Server..")))

(defun warp-start-sending-if-set ()
  (when warp-auto-start-sending
    (warp-start-sending-current-buffer)))

(warp-add-server-listener "start" warp-start-sending-if-set)

(defun warp-stop-sending-current-buffer ()
  "Stop sending html to the server"
  (interactive)
  (progn (when (timerp warp-auto-sending-timer)
           (cancel-timer warp-auto-sending-timer))
         (setq warp-auto-sending-timer nil)))

(warp-add-server-listener "stop" warp-stop-sending-current-buffer)

; Auto Scroll
(defun warp-auto-scroll-running-p ()
  (interactive)
  (timerp warp-auto-scroll-timer))

(defun warp-start-auto-scroll ()
  (interactive)
  (when (not (warp-auto-scroll-running-p))
    (setq warp-auto-scroll-timer
          (run-with-idle-timer
           0.3 t '(lambda ()
                    (when warp-auto-scroll-timer
                      (warp-scroll-to-current-line)))))))

(defun warp-start-auto-scroll-if-set ()
  (when warp-auto-start-auto-scroll
    (warp-start-auto-scroll)))

(warp-add-server-listener "start" warp-start-auto-scroll-if-set)

(defun warp-stop-auto-scroll ()
  (interactive)
  (progn (when (timerp warp-auto-scroll-timer)
           (cancel-timer warp-auto-scroll-timer))
         (setq warp-auto-scroll-timer nil)))

(warp-add-server-listener "stop" warp-stop-auto-scroll)

; Auto Open Client
(defun warp-open-client-if-set ()
  (when warp-auto-open-client
    (unless warp-auto-opened-client-once ; have auto opened
      (warp-open-client) ; not have opened
      (setq warp-auto-opened-client-once t))))

(warp-add-server-listener "start" warp-open-client-if-set)

(defun warp-reset-open-client-flag ()
  (setq warp-auto-opened-client-once nil)) ; clear when server stops

(warp-add-server-listener "stop" warp-reset-open-client-flag)


;;;;;;;;; Provide ;;;;;;;;;
(provide 'warp)