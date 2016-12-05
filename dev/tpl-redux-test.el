;;; tpl-redux-test.el --- Test for tpl.el.

;; Copyright (C) 2016-2017 Rabbit

;; Author: Rabbit <yfhj1990@hotmail.com>
;; Version: 0.0.1
;; Package-Requires: ((dash . "2.13.0") (emacs . "24"))
;; Keywords: template util

;; This file is not part of GUN Emacs.

;;; Code:

(require 'dash)
(require 'tpl-redux)

(ert-deftest test-tpl/redux-syntax-import ()
  "Test tpl/redux-syntax-import."
  (let ((exports '("foo" "bar"))
        (lib "baz")
        (target "import { foo, bar } from 'baz';"))
    (should (equal target
                   (apply 'tpl-redux/redux-syntax-import
                          (-concat `(,lib) exports))))))

(ert-deftest test-tpl/redux-syntax-default-import ()
  "Test tpl/redux-syntax-default-import."
  (let ((exports '("foo" "bar"))
        (lib "baz")
        (default "abc")
        (target "import abc, { foo, bar } from 'baz';"))
    (should (equal target
                   (apply 'tpl-redux/redux-syntax-default-import
                          (-concat `(,lib) `(,default) exports))))))

(ert-deftest test-tpl/redux-syntax-file-import ()
  "Test tpl/redux-syntax-file-import."
  (let ((export "foo")
        (path "path/to/bar.js")
        (target "import foo from 'path/to/bar.js';"))
    (should (equal target (tpl-redux/redux-syntax-file-import path export)))))

(ert-deftest test-tpl/redux-syntax-filetype-import ()
  "Test tpl/redux-syntax-reducer-import."
  (let ((name "foo")
        (path-base "path/to"))
    (let ((target "import fooReducer from 'path/to/foo.reducer';"))
      (should (equal target (tpl-redux/redux-syntax-filetype-import path-base :reducer name))))
    (let ((target "import FOO_BAR from 'path/to/foo-bar.type';"))
      (should (equal target (tpl-redux/redux-syntax-filetype-import path-base :type "foo-bar"))))
    (let ((target "import fooBar from 'path/to/foo-bar.action';"))
      (should (equal target (tpl-redux/redux-syntax-filetype-import path-base :action "foo-bar"))))))

(ert-deftest test-tpl/redux-syntax-reducer-prop-default ()
  ""
  (should (equal "foo: []" (tpl/redux-syntax-reducer-prop-default "foo")))
  (should (equal "foo: []" (tpl/redux-syntax-reducer-prop-default "foo" :list)))
  (should (equal "foo: {}" (tpl/redux-syntax-reducer-prop-default "foo" :map)))
  (should (equal "foo: ''" (tpl/redux-syntax-reducer-prop-default "foo" :string)))
  (should (equal "foo: 1" (tpl/redux-syntax-reducer-prop-default "foo" :number)))
  (should (equal "foo: false" (tpl/redux-syntax-reducer-prop-default "foo" :boolean)))
  (should (equal "foo: 'bar'" (tpl/redux-syntax-reducer-prop-default "foo" nil "'bar'"))))

(ert-deftest test-tpl/redux-syntax-reducer-init ()
  ""
  (let ((prop "foo")
        (type :string)
        (value "'bar'"))
    (should (equal "  foo: []" (tpl/redux-syntax-reducer-init prop)))
    (should (equal "  foo: ''" (tpl/redux-syntax-reducer-init prop type)))
    (should (equal "  foo: 'bar'" (tpl/redux-syntax-reducer-init prop nil value)))))

(ert-deftest test-tpl/redux-syntax-reducer-case ()
  ""
  (let ((type "foo")
        (target "  case FOO:
    return state;\
"))
    (should (equal target (tpl/redux-syntax-reducer-case type))))

  (let ((type "foo")
        (prop "bar")
        (target "  case FOO:
    return Object.assign({}, state, {
      bar: []
    });\
"))
    (should (equal target (tpl/redux-syntax-reducer-case type prop))))

  (let ((type "foo-bar")
        (prop "bar")
        (action "baz")
        (target "  case FOO_BAR:
    return Object.assign({}, state, {
      bar: action.payload.baz
    });\
"))
    (should (equal target (tpl/redux-syntax-reducer-case type prop action)))))

(ert-deftest test-tpl/redux-syntax-reducer-import-types ()
  "Test reducer import types"
  (let ((target "\
import FOO_BAR from 'path/to/foo-bar.type';
import FOO_BAZ from 'path/to/foo-baz.type';
import BAZ from 'path/to/baz.type';\
"))
    (should (equal target
                   (tpl/redux-syntax-reducer-import-types "path/to" "foo-bar" "foo-baz" "baz")))))

(ert-deftest test-tpl/redux-syntax-reducer-define-init-state ()
  "Test reducer define init state."
  (let ((target "\
  foo: [],
  bar: 'baz'\
"))
    (should (equal target
                   (tpl/redux-syntax-reducer-define-init-state '("foo") '("bar" nil "'baz'"))))))

(ert-deftest test-tpl/redux-syntax-reducer-match-types ()
  "Test reducer match types."
  (let ((target "\
  case FOO:
    return state;
  case FOO_BAR:
    return Object.assign({}, state, {
      prop: action.payload.actionType
    });\
"))
    (should (equal target
                   (tpl/redux-syntax-reducer-match-types '("foo") '("foo-bar" "prop" "actionType"))))))

;;; tpl-redux-test.el ends here
