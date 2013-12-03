;;; warp-client.el --- warp client interface

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

(require 'warp-server)
(eval-when-compile (require 'cl))

(defstruct warp-client
  (path (expand-file-name
         "warp-client/index.html"
         (file-name-directory
          (or load-file-name "."))))
  server
  )

(defun warp-client-open (client)
  (browse-url (warp-client-url client)))

(defun warp-client-url (client)
   (concat
    "file://"
    (warp-client-path client)
    "?wsport="
    (number-to-string (warp-server-port
                       (warp-client-server client)))))

(provide 'warp-client)

;;; warp-client.el ends here
