;;; warp.el --- Web Documents Realtime Preview Minor Mode

;; Copyright (C) 2012 by yukihiro hara

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

;; Author: yukihiro hara <yukihr@gmail.com>
;; URL: http://github.com/yukihr/Warp
;; Version: 0.0.2

;;; Contributers:
;;; Installation:
;;; Commentary:
;;; Commands:
;;  See README.md

;;; TODO

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
  "Open client in browser automatically."
  :type 'boolean
  :group 'warp)

(defcustom warp-auto-close-client t
  "Close client when warp server stops."
  :type 'boolean
  :group 'warp)

(defcustom warp-auto-start-sending t
  "Start sending html to the server automatically."
  :type 'boolean
  :group 'warp)

(defcustom warp-auto-start-auto-scroll t
  "Start scroll syncing automatically."
  :type 'boolean
  :group 'warp)

(defcustom warp-idle-time 0.4
  "Time for idle detection on html sending."
  :type 'number
  :group 'warp)

(defcustom warp-server-command "warp"
  "Warp server command."
  :type 'string
  :group 'warp)

(defcustom warp-server-command-args nil
  "Default arguments for warp server command."
  :type 'list
  :group 'warp)

(defcustom warp-base-url "http://localhost:"
  "Base URL to open warp client."
  :type 'string
  :group 'warp)

(defcustom warp-format-converter-alist '()
  "Alist of converters used for converting specific format to html. The format is:

'\(\(\"Filename or Regexp or Major Mode Symbol\" \"Need STDIN Input\" \"Function which returns convert command in list format\"\)..\)

e.g.) Setting for Markdown
\(add-to-list 'warp-format-converter-alist
 '(\"\\.md\\|\\.markdown\" t (lambda ()
                            '(\"markdown\"))))

If `warp-mode' is enabled on buffer its file name matches \"Filename or Regexp\"
or its major-mode equals \"Major Mode Symbol\",
`warp-mode' converts buffer string to HTML using converter command returned by
associated function before sending string to server. If \"Need STDIN Input\" is not nil,
send current buffer string to command's STDIN."
  :type 'list
  :group 'warp)

(defcustom warp-mode-commands-prefix "\C-c\C-w"
  "Prefix keybind for warp-mode commands."
  :type 'string
  :group 'warp)

(defcustom warp-mode-commands-keymap
  '(("s" . 'warp-start-server)
    ("i" . 'warp-stop-server)
    ("o" . 'warp-open-client)
    ("w" . 'warp-send-current-buffer)
    )
  "Keymap for warp-mode.
Each keys are preceded by `warp-mode-commands-prefix'."
  :type 'list
  :group 'warp)

(defvar warp-mode-map (make-sparse-keymap)
  "Key map for Warp minor mode.")

(defun warp-bind-key-to-func (key func)
  (eval `(define-key warp-mode-map
           ,(format "%s%s" warp-mode-commands-prefix key) ,func)))

(defun warp-setup-mode-keybinds ()
  (dolist (el warp-mode-commands-keymap)
    (warp-bind-key-to-func (car el) (cdr el))))

(defvar warp-mode-hook nil
  "Hook for warp mode")

;;;###autoload
(define-minor-mode warp-mode
  "Warp minor mode"
  :lighter " Warp"
  :group   'warp
  :keymap  warp-mode-map
  (if warp-mode
      (progn
        (warp-setup-mode-keybinds)
        (warp-setup-server-hooks)
        (warp-start-server)
        (run-hooks 'warp-mode-hook))
    (warp-stop-server)))


;;;;;;;;; Fundamental ;;;;;;;;;
; Basic Funcs
(defun warp-process-running-p (process)
  "Return if process is running"
  (cond
   ((and (processp process)
        (equal (process-status process) 'run)) t)
   (t nil)))

(defun warp-start-server-process (buffer filter sentinel &rest args)
  "Start warp server and returns server process.
This function takes buffer to which bind server process to.
Pass nil as buffer if you wish no buffer to be bound."
  (let ((process
         (apply 'start-process "warp-server" buffer
                (expand-file-name warp-server-command warp-server-command-path) args)))
    (set-process-query-on-exit-flag process nil)
    (unless (null sentinel)
      (set-process-sentinel process sentinel))
    (unless (null filter)
      (set-process-filter process filter))
    process))

(defun warp-stop-server-process (server)
  (interrupt-process server))

(defun warp-open-client-at-port (port-num)
  (browse-url (concat warp-base-url (number-to-string port-num) "/")))

(defun warp-send-server-string (server string)
  "Send string to warp server's STDIN"
  (progn
      (process-send-string server string)
    ;; (progn (process-send-string warp-server-process string)
    ;;        (message "%s" string)) ;; debug
      ))

(defun warp-send-server-eof (server)
  "Send EOF to warp server's STDIN."
  (if (warp-server-running-p)
      (process-send-eof server)
    (message "Warp: Server not running..")))

(defun warp-send-server-command (server string)
  "Send string as a command data to warp server's STDIN"
  (unless (string-equal "" string)
    (warp-send-server-string server string)
    (warp-send-server-string server (byte-to-string #x00)) ; Send Null to Separate Command
    (warp-send-server-string server "\n") ; Determine Command
    ))

(defun warp-assoc-default-by-string-or-symbol (cons-of-string-and-symbol alist)
  "assoc-default by one of a string or a symbol"
  (assoc-default cons-of-string-and-symbol
                 alist
                 '(lambda (lhs rhs)
                    (cond ((symbolp lhs) (equal lhs (cdr rhs)))
                          ((stringp lhs) (string-match lhs (car rhs)))
                          (t nil)))))

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

(defvar warp-server-start-hook '()
  "Executed when server start.")
(make-variable-buffer-local 'warp-server-start-hook)

(defvar warp-server-stop-hook '()
  "Executed when server stop.")
(make-variable-buffer-local 'warp-server-stop-hook)

; Global Variables
(defvar warp-server-command-path
  (file-name-directory (or load-file-name "."))
  "Directory of warp server command")

(defvar warp-current-server-port
  warp-server-port-base
  "Current port number for server.
Be sure to get port number by `warp-get-server-port'.")


;;;;;;;;; User Commands ;;;;;;;;;
; Server
(defun warp-get-server-port ()
  "Get next port number for server."
  (setq warp-current-server-port (1+ warp-current-server-port)))

(defun warp-server-running-p ()
  (interactive)
  (and (processp warp-server-process)
       (warp-process-running-p warp-server-process)))

(defun warp-server-process-filter (process output)
  "Receive warp server outputs"
  (message "Warp Server: %s" (replace-regexp-in-string "\n+$" "" output)))

(defun warp-server-process-sentinel (process event)
  "Sentinel for warp server process."
  (progn
    ; stop hook
    (when (equal (process-status process) 'exit)
      (run-hooks 'warp-server-stop-hook))))

(defun warp-start-server ()
  "Start warp server for current buffer"
  (interactive)
  (if (not (warp-server-running-p))
      (progn
        (setq warp-server-port (warp-get-server-port)
              warp-server-process
              (apply 'warp-start-server-process
                     (current-buffer)
                     'warp-server-process-filter
                     'warp-server-process-sentinel
                     "-p" (number-to-string warp-server-port)
                     (append warp-server-command-args
                             (if warp-auto-close-client '("-c") nil))))
                                        ; start hook
        ; start hook
        (let ((exitp
               (catch 'break
                 (while (not (equal (process-status warp-server-process) 'run))
                   (when (equal (process-status warp-server-process) 'exit)
                     (throw 'break t))
                   (sleep-for 0 10))))) ; wait till start
          (if (not exitp)
              (run-hooks 'warp-server-start-hook)
            (message "Warp: Process abnormally exited."))))
    nil))

(defun warp-stop-server ()
  "Stop warp server"
  (interactive)
  (when (warp-server-running-p)
    (warp-stop-server-process warp-server-process)))


; Client
(defun warp-open-client ()
  "Open Warp client within default browser"
  (interactive)
  (warp-open-client-at-port warp-server-port))

; Server Command
(defun warp-send-command (string)
  "Send string as a command data to warp server's STDIN"
  (interactive "sCommand string send to warp: ")
  (warp-send-server-command warp-server-process string))

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

(defun warp-get-converter-settings-for-current-buffer ()
  "Retrive converter settings for current buffer"
  (interactive)
  (warp-assoc-default-by-string-or-symbol
   (cons buffer-file-name major-mode)
   warp-format-converter-alist))

(defun warp-send-current-buffer-converting ()
  "Send warp server current buffer content converting to HTML data."
  (interactive)
  (save-restriction
    (widen)
    (let* (html-message
           (convert-options
            (warp-get-converter-settings-for-current-buffer))
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

(defun warp-current-buffer-need-convert-p ()
  "See if conversion is needed for current buffer. TODO: Refactor, this function is not needed"
  (not (null (warp-get-converter-settings-for-current-buffer))))

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
   (concat
    (byte-to-string #x1B)
    "scroll"
    (byte-to-string #x1D)
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
      (setq warp-last-modified-tick -1
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
                    (warp-send-current-buffer))))))
    (message "Warp: Already Sending to Server..")))

(defun warp-start-sending-if-set ()
  (when warp-auto-start-sending
    (warp-start-sending-current-buffer)))

(defun warp-stop-sending-current-buffer ()
  "Stop sending html to the server"
  (interactive)
  (progn (when (timerp warp-auto-sending-timer)
           (cancel-timer warp-auto-sending-timer))
         (setq warp-auto-sending-timer nil)))


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

(defun warp-stop-auto-scroll ()
  (interactive)
  (progn (when (timerp warp-auto-scroll-timer)
           (cancel-timer warp-auto-scroll-timer))
         (setq warp-auto-scroll-timer nil)))


; Auto Open Client
(defun warp-open-client-if-set ()
  (when warp-auto-open-client
    (unless warp-auto-opened-client-once ; have auto opened
      (warp-open-client) ; not have opened
      (setq warp-auto-opened-client-once t))))

(defun warp-reset-open-client-flag ()
  (setq warp-auto-opened-client-once nil)) ; clear when server stops

;;;;;;;;; hooks ;;;;;;;;;
(defun warp-setup-server-hooks ()
  (progn
    (add-hook 'warp-server-start-hook 'warp-start-sending-if-set)
    (add-hook 'warp-server-stop-hook 'warp-stop-sending-current-buffer)
    (add-hook 'warp-server-start-hook 'warp-start-auto-scroll-if-set)
    (add-hook 'warp-server-stop-hook 'warp-stop-auto-scroll)
    (add-hook 'warp-server-start-hook 'warp-open-client-if-set)
    (add-hook 'warp-server-stop-hook 'warp-reset-open-client-flag)
    ; teardown
    (add-hook 'warp-server-stop-hook 'warp-teardown-server-hooks)))

(defun warp-teardown-server-hooks ()
  (progn
    (remove-hook 'warp-server-start-hook 'warp-start-sending-if-set t)
    (remove-hook 'warp-server-stop-hook 'warp-stop-sending-current-buffer t)
    (remove-hook 'warp-server-start-hook 'warp-start-auto-scroll-if-set t)
    (remove-hook 'warp-server-stop-hook 'warp-stop-auto-scroll t)
    (remove-hook 'warp-server-start-hook 'warp-open-client-if-set t)
    (remove-hook 'warp-server-stop-hook 'warp-reset-open-client-flag t)
    ; remove self
    (remove-hook 'warp-server-stop-hook 'warp-teardown-server-hooks t)))

;;;;;;;;; Provide ;;;;;;;;;
(provide 'warp)

;;; warp.el ends here
