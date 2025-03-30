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

(defun initialize (&optional (logger-name "default"))
  "Initialize log4j and the supporting OpenLDK runtime."
  (openldk:initialize)

  ;; Debug toggles
  ;; (setf openldk::*debug-codegen* t)
  (setf openldk::*debug-load* t)

  ;; Load core classes
  (assert (openldk::classload "org/apache/logging/log4j/LogManager"))
  (assert (openldk::classload "org/apache/logging/log4j/Logger"))
  (assert (openldk::classload "org/apache/logging/log4j/ThreadContext"))

  (setf *logger* (get-logger logger-name)))

(defun get-logger (name)
  "Return a logger instance for NAME (a string)."
  (openldk::|org/apache/logging/log4j/LogManager.getLogger(Ljava/lang/String;)|
   (openldk::jstring name)))

(defun format-log-message (&rest args)
  "Formats log message based on flexible argument rules."
  (cond
    ((and (= (length args) 1)
          (stringp (first args)))
     (first args))
    ((and (> (length args) 1)
          (stringp (first args)))
     (apply #'format nil args))
    ((= (length args) 1)
     (format nil "~A" (first args)))
    (t
     (format nil "~{~A~^ ~}" args)))) ; fall back: space-separated

(defun log-info (&rest args)
  (openldk::|info(Ljava/lang/String;)| *logger*
            (openldk::jstring (apply #'format-log-message args))))

(defun log-debug (&rest args)
  (openldk::|debug(Ljava/lang/String;)| *logger*
             (openldk::jstring (apply #'format-log-message args))))

(defun log-error (&rest args)
  (openldk::|error(Ljava/lang/String;)| *logger*
             (openldk::jstring (apply #'format-log-message args))))

(defun log-warn (&rest args)
  (openldk::|warn(Ljava/lang/String;)| *logger*
            (openldk::jstring (apply #'format-log-message args))))

(defun log-trace (&rest args)
  (openldk::|trace(Ljava/lang/String;)| *logger*
             (openldk::jstring (apply #'format-log-message args))))

(defun log-fatal (&rest args)
  (openldk::|fatal(Ljava/lang/String;)| *logger*
             (openldk::jstring (apply #'format-log-message args))))

;;; Log level checks
(defun trace-enabled-p ()
  (openldk::|isTraceEnabled()| *logger*))

(defun debug-enabled-p ()
  (openldk::|isDebugEnabled()| *logger*))

(defun info-enabled-p ()
  (openldk::|isInfoEnabled()| *logger*))

(defun warn-enabled-p ()
  (openldk::|isWarnEnabled()| *logger*))

(defun error-enabled-p ()
  (openldk::|isErrorEnabled()| *logger*))

(defun fatal-enabled-p ()
  (openldk::|isFatalEnabled()| *logger*))

;;; Structured logging
(defun log-with-fields (level fields message)
  "Log a MESSAGE at LEVEL with a list of key-value FIELDS."
  (let ((builder (openldk::|atLevel(Lorg/apache/logging/log4j/Level;)|
                   *logger*
                   (openldk::|org/apache/logging/log4j/Level.valueOf(Ljava/lang/String;)|
                    (openldk::jstring (string-upcase level))))))
    (dolist (pair fields)
      (openldk::|with(Ljava/lang/String;Ljava/lang/Object;)|
       builder
       (openldk::jstring (string (car pair)))
       (openldk::jstring (string (cdr pair)))))
    (openldk::|log(Ljava/lang/String;)| builder (openldk::jstring message))))

;;; MDC (Mapped Diagnostic Context)
(defun mdc-put (key value)
  (openldk::|org/apache/logging/log4j/ThreadContext.put(Ljava/lang/String;Ljava/lang/String;)|
   (openldk::jstring key)
   (openldk::jstring value)))

(defun mdc-get (key)
  (openldk::lstring (openldk::|org/apache/logging/log4j/ThreadContext.get(Ljava/lang/String;)|
                              (openldk::jstring key))))

(defun mdc-remove (key)
  (openldk::|org/apache/logging/log4j/ThreadContext.remove(Ljava/lang/String;)|
            (openldk::jstring key)))

(defun mdc-clear ()
  (openldk::|org/apache/logging/log4j/ThreadContext.clearAll()|))

(defun configure-from-file (path)
  (assert (openldk::classload "org/apache/logging/log4j/core/config/Configurator"))
  (openldk::|org/apache/logging/log4j/core/config/Configurator.initialize(Ljava/lang/String;)|
            (openldk::jstring path)))

(defun mdc-push (message)
  "Push a message onto the ThreadContext stack."
  (openldk::|org/apache/logging/log4j/ThreadContext.push(Ljava/lang/String;)|
            (openldk::jstring message)))

(defun mdc-pop ()
  "Pop the top message from the ThreadContext stack and return it."
  (openldk::lstring (openldk::|org/apache/logging/log4j/ThreadContext.pop()|)))

(defmacro with-thread-context (message &body body)
  "Push MESSAGE onto ThreadContext stack for the duration of BODY."
  `(unwind-protect
       (progn
         (mdc-push ,message)
         ,@body)
     (mdc-pop)))
