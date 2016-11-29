;;; redux.el --- Generator for redux/react app.

;; Copyright (C) 2016-2017 Rabbit

;; Author: Rabbit <yfhj1990@hotmail.com>
;; Version: 0.0.1
;; Package-Requires: ((s . "1.10.0") (emacs . "24"))
;; Keywords: redux, react

;; This file is not part of GUN Emacs.

;;; Code:

(require 'dash)
(require 's)
(require 'f)


(defun type-tostring (type)
  "\
Convert type to string.
"
  (s-chop-prefix ":" (symbol-name type)))


(defvar redux/store '())

(defun redux/find-root-path (root-file current-dir)
  "\
Find root path.
"
  (let ((target (f-expand root-file current-dir)))
    (if (not (f-exists? target))
        ;;
        (progn (read-string (concat "Root file \"" root-file "\" not find in "
                                    current-dir
                                    ", upwards find it (y/n)? ") nil nil "y")
               (let ((find-path (f-traverse-upwards
                                 (lambda (path)
                                   (f-exists? (f-expand root-file path)))
                                 current-dir)))
                 (if (equal nil find-path)
                     ;; Not find.
                     (error (concat "Con't find file " root-file))
                   ;; To store.
                   (plist-put redux/store :root find-path))))

      ;; Return current-dir
      (plist-put redux/store :root current-dir))))


(defvar redux/target-list '((:dir "reducers")
                            (:dir "types")
                            (:dir "actions")
                            (:dir "containers")
                            (:dir "components")
                            (:dir "selectors")
                            (:dir "styles")
                            (:dir "middlewares")
                            (:dir "store")
                            (:file "index.js")
                            (:file "routes.js")
                            (:file "configureStore.js" :parents "store")
                            (:file "app.react.js" :parents "containers")
                            (:file "reducers.js.example" :parents "reducers"))
  "Init targets")



(defun redux/init-dir-and-files (root-path target-dir)
  "\
Create init dir and files.
"
  (let ((target-path (f-join root-path target-dir)))
    (-map (lambda (target)
            (let ((type (car target)))
              ;;(print type)
              (cond ((eq :dir type)
                     (f-mkdir (f-join target-path (plist-get target :dir))))
                    ((eq :file type)
                     (if (eq nil (plist-get target :parents))
                         (f-touch (f-join target-path (plist-get target :file)))
                       (f-touch (f-join target-path
                                        (plist-get target :parents)
                                        (plist-get target :file)))))
                    (t (error (concat "Unknow type " type))))))
          redux/target-list)))


(defun redux/reducer-replace-template (&optional options)
  (let* ((template (f-read "./templates/combine-reducer.js.template"))
         (replace-importor `("{{type-imports}}" . ,(s-concat (s-join "\n" mapped-importor) "\n")))
         (replace-props `("{{props}}" . ,(s-join ",\n" mapped-props)))
         )
    (s-replace-all (list replace-importor
                         replace-props)
                   template)))


(defun redux/reducer-type-import (target-path type)
  "\
"
  (let ((exportor (s-upcase type))
        (filename (concat type ".type.js")))
    (concat "import "
            exportor
            " from "
            "'"
            (f-join target-path "types" filename)
            "';")))

(defun redux/reducer-type-imports (target-path &rest types)
  "\
"
  (s-join "\n"
         (-map (lambda (type)
                 (redux/reducer-type-import target-path type))
               types)))

(defun redux/reducer-init-state (name &optional type)
  "\
"
  (cond ((eq type :string)
         (concat "  " name ": " "''"))
        ((eq type :number)
         (concat "  " name ": " "1"))
        ((eq type :boolean)
         (concat "  " name ": " "false"))
        ((eq type :map)
         (concat "  " name ": " "{}"))
        (t (concat "  " name ": " "[]"))))

(defun redux/reducer-init-states (&rest args)
  "\

"
  (s-join ",\n"
          (-map (lambda (option)
                  (apply 'redux/reducer-init-state option))
                args)))



(defun redux/reducer-type-case (type &optional prop propType actionProp)
  "\
Replace reducer case and state modify.

1. Simple type, only return state itself.

example:

  case FOO:
    return state;

2. Type and prop.

example:

  case FOO:
    return Object.assign({}, state, {
      bar: []
    });

3. Modifie Prop with action prop.

example:

  case FOO:
    return Object.assign({}, state, {
      bar: action.payload.baz
    });
"
  (if (not type)
      (error (concat "Unknow type " type))
    (if (not prop)
        ;; Normal prop.
        (concat "    case "
                (s-upcase type)
                ":\n"
                "      return state;")
      (if (not (eq :action propType))
          ;; Merge prop.
          (concat "    case "
                  (s-upcase type)
                  ":\n"
                  "      return Object.assign({}, state, {\n      "
                  (apply 'redux/reducer-init-state `(,prop ,propType))
                  "\n      });")
        ;; Action
        (concat "    case "
                (s-upcase type)
                ":\n"
                "      return Object.assign({}, state, {\n        "
                prop
                ": "
                "action.payload."
                actionProp
                "\n      });")))))


(defun redux/reducer-type-cases (&rest args)
  "\
Replace reducer type cases list.
"
  (s-join "\n"
          (-map (lambda (option)
                  (apply 'redux/reducer-type-case option))
                args)))


(provide 'redux)

;;; redux.el ends here
