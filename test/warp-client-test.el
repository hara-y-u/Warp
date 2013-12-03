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

(require 'ert)
(require 'warp-client)
(eval-when-compile (require 'cl))

(defstruct warp-server
  port)

(ert-deftest warp-client-url ()
  (let* ((warp-server (make-warp-server
                       :port 9998))
         (warp-client (make-warp-client
                      :server warp-server)))
    (should
     (string-match "^file://.+\?wsport=[[:digit:]]+"
                   (warp-client-url warp-client)))))

