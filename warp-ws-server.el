;;; warp-ws-server.el --- warp server

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
(require 'websocket)
;(require 'warp-converter)

(defstruct (warp-ws-server
            (:constructor nil)
            (:constructor warp-ws-server--inner-make))
  (port 8850)
  websocket-server
  warp-web-server
  (sockets nil)
  )

(defun make-warp-ws-server (&rest options)
  (let* ((ws-server
          (apply 'warp-ws-server--inner-make options)))
    )
  )


(provide 'warp-ws-server)

;;; warp-ws-server.el ends here
