;;; tpl-redux.el --- Template utils.

;; Copyright (C) 2016-2017 Rabbit

;; Author: Rabbit <yfhj1990@hotmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash . "2.13.0") (s . "1.10.0") (emacs . "24"))
;; Keywords: template util

;; This file is not part of GUN Emacs.

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'tpl)

(defun tpl-redux/redux-syntax-import (libname &rest exports)
  "Base import."
  (let ((concat-exports (if (not exports) "" (s-join ", " exports))))
    (tpl/render (tpl/reader "redux-import")
                `((lib . ,libname) (exports . ,concat-exports)))))

(defun tpl-redux/redux-syntax-default-import (libname default &rest exports)
  "Import with default export."
  (let ((concat-exports (if (not exports) "" (s-join ", " exports))))
    (tpl/render (tpl/reader "redux-default-import")
                `((lib . ,libname) (exports . ,concat-exports) (default . ,default)))))

(defun tpl-redux/redux-syntax-file-import (path export)
  "User file export."
  (tpl/render (tpl/reader "redux-file-import")
              `((path . ,path) (export . ,export))))

(defun tpl-redux/redux-syntax-filetype-import (path-base type name)
  "Import reducer."
  (cond ((eq type :reducer)
         (tpl-redux/redux-syntax-file-import (f-join path-base (concat name ".reducer")) (concat name "Reducer")))
        ((eq type :type)
         (tpl-redux/redux-syntax-file-import (f-join path-base (concat name ".type")) (s-upcase (s-snake-case name))))
        ((eq type :action)
         (tpl-redux/redux-syntax-file-import (f-join path-base (concat name ".action")) (s-lower-camel-case name)))
        (t (error (concat "Unknow type: " type)))))

(defun tpl/redux-syntax-reducer-prop-default (prop &optional type value)
  "Default prop key-value pair.
TODO value type.
"
  (if value (concat prop ": " value)
    (cond ((eq type :string)
           (concat prop ": " "''"))
          ((eq type :number)
           (concat prop ": " "1"))
          ((eq type :boolean)
           (concat prop ": " "false"))
          ((eq type :map)
           (concat prop ": " "{}"))
          (t (concat prop ": " "[]")))))

(defun tpl/redux-syntax-reducer-init (&rest args)
  "Init state."
  (concat "  " (apply 'tpl/redux-syntax-reducer-prop-default args)))

(defun tpl/redux-syntax-reducer-case (type &optional prop action)
  (let ((upper-type (s-upcase (s-snake-case type))))
    (if (not prop) (tpl/render (tpl/reader "redux-reducer-default-case") `((type . ,upper-type)))
      (if action (tpl/render (tpl/reader "redux-reducer-action-case") `((type . ,upper-type) (prop . ,prop) (action . ,action)))
        (tpl/render (tpl/reader "redux-reducer-prop-case") `((type . ,upper-type) (prop . ,prop)))))))

(defun tpl/redux-syntax-reducer-import-types (base-path &rest types)
  "Import types."
  (s-join "\n"
          (-map (lambda (type)
                  (funcall 'tpl-redux/redux-syntax-filetype-import base-path :type type))
                types)))

(defun tpl/redux-syntax-reducer-define-init-state (&rest prop-pairs)
  "Define init states."
  (s-join ",\n"
          (-map (lambda (args)
                  (apply 'tpl/redux-syntax-reducer-init args))
                prop-pairs)))

(defun tpl/redux-syntax-reducer-match-types (&rest case-opts)
  "Define init states."
  (s-join "\n"
          (-map (lambda (args)
                  (apply 'tpl/redux-syntax-reducer-case args))
                case-opts)))

(provide 'tpl-redux)

;;; tpl-redux.el ends here
