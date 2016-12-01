;;; tpl.el --- Template utils.

;; Copyright (C) 2016-2017 Rabbit

;; Author: Rabbit <yfhj1990@hotmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash . "2.13.0") (s . "1.10.0") (emacs . "24"))
;; Keywords: template util

;; This file is not part of GUN Emacs.

;;; Code:

(require 'dash)
(require 's)

(defvar placeholder-regex "{{\\([^}]+\\)}}")

(defun tpl/pick-placeholders (template)
  "\
Pick all of placeholders.
"
  (s-match-strings-all placeholder-regex
                       template))

(defun tpl/replace-placeholds (template data-list)
  "\
Make replace pairs alist for render.
"
  (let ((pick-list (tpl/pick-placeholders template)))

    (-map (lambda (picked)
            (let ((placeholder (car picked))
                  (name (cadr picked)))
              `(,placeholder . ,(alist-get (intern name) data-list))
              ))
          pick-list)))

(defun tpl/render (template data-list)
  "\
Render template by datas.
"
  (let ((replacers (tpl/replace-placeholds template data-list)))
    (s-replace-all replacers template)))

(provide 'tpl)

;;; tpl.el ends here
