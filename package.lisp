;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LOG4J; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This file is part of cl-log4j

(defpackage #:log4j
  (:use #:cl)
  (:export :initialize
           :configure-from-file

           :log-info
           :log-debug
           :log-error
           :log-warn
           :log-trace
           :log-fatal

           :info-enabled-p
           :debug-enabled-p
           :error-enabled-p
           :warn-enabled-p
           :trace-enabled-p
           :fatal-enabled-p

           :mdc-put
           :mdc-get
           :mdc-remove
           :mdc-clear

           :with-thread-context
           ))
