;;; warp-client-test.el --- Unit tests for warp-client

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

;; TODO: Test will affect another unit test

(eval-when-compile (require 'cl))
(require 'ert)
(require 'warp-client)


;;; Mock

(defstruct warp-ws-server
  port)

(defstruct warp-client-server
  host
  port
  warp-ws-server)

(setq test-port 8802)

(setq warp-ws-server (make-warp-ws-server
                      :port 9998))

(setq warp-client-server (make-warp-client-server
                          :host "localhost"
                          :port test-port
                          :warp-ws-server warp-ws-server))

;;; Helpers

(defun purge-web-server ()
  (elnode-stop test-port))

;;; Tests

(ert-deftest warp-client-open ()
  (warp-client-open warp-client-server)
  (should
   (y-or-n-p "Client url opened in browser correctly?"))
  (purge-web-server)
  )
