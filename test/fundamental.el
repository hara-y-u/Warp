;; test for fundamental functions

(require 'ert)
(require 'warp)

(ert-deftest warp-test-assoc-default-by-string-or-symbol ()
  (let ((alist '(("foo" "foo")
                 (bar "bar")
                 ("\\.baz" "baz"))))
    (should (equal (warp-assoc-default-by-string-or-symbol '("foo" . foo) alist) '("foo")))
    (should (equal (warp-assoc-default-by-string-or-symbol '("bar" . bar) alist) '("bar")))
    (should (equal (warp-assoc-default-by-string-or-symbol '("hoge.baz" . baz) alist) '("baz")))
    ))
