;;; log4j.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;

(in-package :log4j)

(defvar *logger* nil)

(defun initialize ()
  "Initialize log4j and the supporting OpenLDK runtime."

  ;; Initialize the OpenLDK runtime
  (openldk:initialize)

  ;; Uncomment this to see generated lisp code
  ;; (setf openldk::*debug-codegen* t)

  ;; Comment this to silence class loading
  (setf openldk::*debug-load* t)

  (assert (openldk::classload "org/apache/logging/log4j/LogManager"))

  (setf *logger*
        (openldk::|org/apache/logging/log4j/LogManager.getLogger()|)))

(defun log-error (message)
  "Log an error MESSAGE."
  (openldk::|error(Ljava/lang/String;)| *logger* (openldk::jstring message)))
