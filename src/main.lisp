;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/debug)

(defparameter *lack-middleware-debug*
  (lambda (app &key (output *standard-output*)
                 special-variables
                 stylesheet-hrefs
                 (print-env t)
                 (print-backtrace t)
                 (redact-backtrace t)
                 (include-special-variables-html t)
                 (include-backtrace-html t))
    (declare (type function app))
    (check-type output (or null stream pathname))
    (check-type print-env boolean)
    (check-type print-backtrace boolean)
    (when (or print-env print-backtrace)
      (assert output
              nil
              "OUTPUT must be set when PRINT-ENV or PRINT-BACKTRACE is T"))
    (check-type redact-backtrace boolean)
    (check-type special-variables list)
    (check-type stylesheet-hrefs list)
    (dolist (var-sym special-variables)
      (check-type var-sym symbol))
    (dolist (href stylesheet-hrefs)
      (check-type href string))
    (check-type include-special-variables-html boolean)
    (check-type include-backtrace-html boolean)
    (lambda (env)
      (declare (type list env)
               (type boolean include-backtrace-html))
      (block nil
        (handler-bind ((error (lambda (condition)
                                (declare (type error condition))
                                (when output
                                  (if redact-backtrace
                                      (foo.lisp.redact/backtrace:print-error
                                       condition env output print-env print-backtrace redact-backtrace)
                                      (trivial-backtrace:print-backtrace condition :output output)))
                                (return
                                  `(500
                                    (:content-type "text/html")
                                    (,(debug-mode-html
                                       condition
                                       env
                                       :stylesheet-hrefs stylesheet-hrefs
                                       :special-variables (and include-special-variables-html
                                                               special-variables)
                                       :include-backtrace include-backtrace-html
                                       :redact-backtrace redact-backtrace)))))))
          (funcall app env)))))
  "Lack middleware for rendering a debug page")

(defun debug-mode-html (condition env &key include-backtrace
                                        redact-backtrace
                                        special-variables
                                        stylesheet-hrefs)
  (let ((request-id (getf env :request-id)))
    (spinneret:with-html-string (:doctype)
      (:html
       (:head
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width,initial-scale=1")
        (:meta :name "turbo-visit-control" :content "reload")
        (:title "Lack Middleware Debug")
        (loop for href in stylesheet-hrefs
              do (:link :rel "stylesheet" :type "text/css" :href href)))
       (:body
        :class "debug-mode"
        (:h1 (format nil "~A" (type-of condition)))
        (:p (format nil "An error occurred while processing a request: ~A ~A"
                    (getf env :request-method)
                    (getf env :request-uri)))
        (when request-id
          (:p (format nil "Request ID: ~A" request-id)))
        (:hr)
        (:h2 :class "debug-mode__error-message-title"
             "Error Message")
        (:pre :style "white-space: pre-wrap" :class "debug-mode__error-message-content"
              (format nil "~A" condition))
        (:h2 :class "debug-mode__env-title"
             "ENV")
        (:pre :style "white-space: pre-wrap" :class "debug-mode__env-content"
              (foo.lisp.redact:print-env env nil))
        (when special-variables
          (:h2 "Special Variables")
          (:table
           :border "1"
           (:thead
            (:tr
             (:td "Variable")
             (:td "BOUNDP")
             (:td "Value")
             (:td "Type")))
           (:tbody
            (dolist (variable-sym special-variables)
              (:tr
               (:td (format nil "~A" variable-sym))
               (:td (format nil "~A" (boundp variable-sym)))
               (:td (if (boundp variable-sym)
                        (format nil "~A" (symbol-value variable-sym))
                        "-"))
               (:td (if (boundp variable-sym)
                        (format nil "~A" (type-of (symbol-value variable-sym)))
                        "-")))))))
        (when include-backtrace
          (progn
            (:h2 :class "debug-mode__backtrace-title" "Backtrace")
            (:pre :style "white-space: pre-wrap" :class "debug-mode__backtrace-content"
                  (if redact-backtrace
                      (let ((stream (make-string-output-stream)))
                        (foo.lisp.redact/backtrace:print-error
                         condition env stream nil t redact-backtrace)
                        (get-output-stream-string stream))
                      (trivial-backtrace:print-backtrace condition :output nil)))))
        (:h2 :class "debug-mode__system-information-title" "System Information")
        (:pre :class "debug-mode__system-information-content"
              (format nil "~A" (cl-info:get-cl-info))))))))
