;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.lack-middleware-debug"
  :version "0.0.1"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("cl-info"
               "foo.lisp.http-response"
               "foo.lisp.redact"
               "spinneret")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Lack middleware to display a debug page on application errors and HTTP error responses"
  :in-order-to ((test-op (test-op "foo.lisp.lack-middleware-debug/tests"))))

(defsystem "foo.lisp.lack-middleware-debug/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.lack-middleware-debug"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.lack-middleware-debug"
  :perform (test-op (op c) (symbol-call :rove :run c)))
