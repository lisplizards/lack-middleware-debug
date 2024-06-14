;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:lack/middleware/debug/tests/main
  (:use #:cl #:rove)
  (:import-from #:lack/middleware/debug
                #:*lack-middleware-debug*))
