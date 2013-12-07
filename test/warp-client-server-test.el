;;; warp-client-server-test.el --- Unit tests for warp-client-server

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
(require 'warp-client-server)

;;; Server Mock

(defstruct warp-ws-server
  port)


;;; Variables

(setq warp-ws-server (make-warp-ws-server
                     :port 9998))

(setq test-port 8803)


;;; Helpers

(defun purge-web-server ()
  (elnode-stop test-port))


;;; Tests

(ert-deftest make-warp-client-server ()
  (let ((client-server (make-warp-client-server
                        :port test-port
                        :warp-ws-server warp-ws-server)))
    (should
     (eq test-port
         (warp-client-server-port client-server)))
    (should
     (equal "localhost"
         (warp-client-server-host client-server)))
    (purge-web-server)
    ))

(ert-deftest warp-client-server-url ()
  (let ((client-server (make-warp-client-server
                        :port test-port
                        :warp-ws-server warp-ws-server)))
    (should
     (string-match "^http://localhost:[[:digit:]]+\\?wsport=[[:digit:]]+"
                   (warp-client-server-url client-server)))
    (purge-web-server)
    ))

(ert-deftest delete-warp-client-server ()
  (let ((client-server (make-warp-client-server
                        :port test-port
                        :warp-ws-server warp-ws-server)))
    (sleep-for 0.5)
    (delete-warp-client-server client-server)
    (sleep-for 0.5)
    (should
     (null
      (elnode-stop test-port)))
    (purge-web-server)
    ))

;; TODO: tests for handlers
