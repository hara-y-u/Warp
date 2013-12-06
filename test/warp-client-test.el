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

;; Server Mock
(defstruct warp-server
  port)

(defvar warp-server (make-warp-server
                     :port 9998))

(defvar warp-client-server (make-warp-client-server
                     :server warp-server))

(ert-deftest warp-client-url ()
  (should
   (string-match "^file://.+\?wsport=[[:digit:]]+"
                 (warp-client-url warp-client))))

(ert-deftest warp-client-open ()
  (warp-client-open warp-client)
  (should
   (y-or-n-p "Client opened in browser correctly?")))

