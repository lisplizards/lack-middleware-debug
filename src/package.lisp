;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:lack/middleware/debug
  (:use #:cl)
  (:import-from #:foo.lisp.http-response
                #:status-code
                #:status-code-error
                #:status-code-to-text)
  (:export #:*lack-middleware-debug*))
