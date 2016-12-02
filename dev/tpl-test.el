;;; tpl-test.el --- Test for tpl.el.

;; Copyright (C) 2016-2017 Rabbit

;; Author: Rabbit <yfhj1990@hotmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs . "24"))
;; Keywords: template util

;; This file is not part of GUN Emacs.

;;; Code:


(require 'tpl)

(ert-deftest test-tpl/pick-placeholders ()
  "Test tpl/pick-placeholders."
  (let ((template "{{foo}}")
        (target '(("{{foo}}" "foo"))))
    (should (equal target (tpl/pick-placeholders template)))))


(ert-deftest test-tpl/replace-placeholders()
  "Test tpl/replace-placeholders."
  (let ((template "{{foo}}")
        (data '((foo . "bar")))
        (target '(("{{foo}}" . "bar"))))
    (should (equal target (tpl/replace-placeholders template data)))))

(ert-deftest test-tpl/render ()
  "Test tpl/render."
  (let ((template "{{foo}}")
        (data '((foo . "bar")))
        (target "bar"))
    (should (equal target (tpl/render template data)))))


;;; tpl-test.el ends here
