;;; warp-reload.el --- Reload Utility for Warp
;; -*- mode: emacs-lisp -*-

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

;;; Version: 0.0.1
;;  Author: yukihr (yukihiro hara), <yukihr [at] gmail [dot] com>
;;  URL: http://yukihr.github.com/

;;; Contributers:
;;; Installation:
;;; Commentary:
;;; Commands:
;;; Todo:
;;;    * More customization availability.
;;;    * Display URL on mode line.

(require 'warp)

(defcustom warp-reload-default-base-url "http://localhost:"
  "Default url to query URL.")

(defvar warp-reload-mode-hook nil
  "Hook for warp-reload mode")

(defvar warp-reload-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-w C-c") 'warp-reload-change-url)
    (define-key map (kbd "C-c C-w C-o") 'warp-reload-open-client)
    map))

;;;###autoload
(define-minor-mode warp-reload
  "Warp Reload minor mode"
  :lighter " Warp-R"
  :group  'warp
  :global t
  :keymap warp-reload-map
  (if warp-reload
      (progn
        (warp-reload-start-server)
        (setq warp-reload-url (car (warp-reload-query-url)))
        (warp-reload-open-client)
        (warp-reload-start-reloading-at-save)
        (run-hooks 'warp-reload-mode-hook))
    (progn
      (warp-reload-stop-reloading-at-save)
      (warp-reload-stop-server)
      )))


(defvar warp-reload-server-process nil
  "Global Server Process. Used by warp-reload-mode.")

(defvar warp-reload-server-port nil
  "Global Server Port. Used by warp-reload-mode.")


(defun warp-reload-start-server ()
  "Start Warp server for Warp-Reload mode."
  (interactive)
  (when (or (null warp-reload-server-process)
            (not (equal (process-status warp-reload-server-process) 'run)))
    (setq warp-reload-server-port (warp-get-server-port))
    (set 'warp-reload-server-process
         (warp-start-server-process
          "*warp-reload-server*"
          nil
          nil
          "-p" (number-to-string warp-reload-server-port) "-c" "-h"))))

(defun warp-reload-stop-server ()
  (interactive)
  (warp-stop-server-process warp-reload-server-process))

(defun warp-reload-open-client ()
  "Open Warp client for Warp-Reload mode."
  (interactive)
  (warp-open-client-at-port warp-reload-server-port))

(defvar warp-reload-url-history '()
  "URL history.")

(defvar warp-reload-url nil
  "Current url for reloading.")

(defun warp-reload-query-url ()
  (let ((url (read-string
              "URL: "
              (or warp-reload-url
                  warp-reload-default-base-url)
              'warp-reload-url-history
              nil
              )))
    (list (if (null (string-match "^\\(http://\\|file://\\)" url))
            (concat "http://" url)
          url))))

(defun warp-reload-change-url (url)
  "Change url for reloading."
  (interactive (warp-reload-query-url))
  (progn
    (setq warp-reload-url url)
    (warp-reload-load-current-url)))

(defun warp-reload-load-current-url ()
  "Load currently set url."
  (interactive)
  (warp-send-server-command warp-reload-server-process
                            (concat "load" warp-reload-url)))

(defun warp-reload-request-reload ()
  "Reload client."
  (interactive)
  (warp-send-server-command warp-reload-server-process "reload"))

(defun warp-reload-start-reloading-at-save ()
  (interactive)
  (progn (warp-reload-load-current-url)
         (add-hook 'after-save-hook 'warp-reload-request-reload)))

(defun warp-reload-stop-reloading-at-save ()
  (interactive)
  (remove-hook 'after-save-hook 'warp-reload-request-reload))

(provide 'warp-reload)
;;; warp-reload.el ends here
