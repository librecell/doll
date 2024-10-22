#|----------------------------|#
#|--doll----an-offering-from--|#
#|--------------librecell-----|#
#|----------------------------|#

(ql:quickload '(:cl-ppcre :telnetlib))

#| quicklisp dependencies --^  |#
#|    v-- package definition   |#

(defpackage doll
  "open maintain and handle a persistent socket stream"
  (:use :cl :sb-thread :cl-ppcre :telnetlib)
  (:export defbind
           defthread
           version *version* doll-version *doll-version* doll/version
           hostname *hostname*
           port *port*
           connection *connection*
           connect-to-host
           listener-loop
           format-to-connection
           +build-development-version-of-doll+
           listener-loop-hacky
           doll doll1 doll2))


(in-package :doll)

#|----------------------------|#
#| generics/methods by macros |#
#|----------------------------|#

(defmacro defbind (name value)
  "global parameter and generic method to retreive it by funcall"
  `(progn
     (defparameter ,(intern (concatenate 'string "*" (string name) "*")) ,value)
     (defgeneric ,name ())
     (defmethod ,name ()
       (symbol-value ',(intern (concatenate 'string "*" (string name) "*"))))))

(defmacro defthread (&optional (name nil) &body body)
  "when a name is provided, it and body is memoized in an association table"
  (let ((thread-name (if name
                         (concatenate 'string "defined-thread-" (string name) "-")
                         "defined-thread-"))
        (timestamp (get-internal-real-time)))
    `(progn
       (defun ,(intern (concatenate 'string "init-" (string name))) ()
         (setf ,(intern (concatenate 'string "*thread-" thread-name (format nil "~D" timestamp) "*"))
               (make-thread (lambda () ,@body) :name ,(concatenate 'string thread-name (format nil "~D" timestamp))))))))


(defbind version (sb-posix:time))
(defbind doll-version (cons 2.001 #\B))

(defun doll/version ()
  "all versions and timestamps as values"
  (values (cons (version) (doll-version))
          (version)
          *version*
          (doll-version)
          (sb-posix:time)
          (get-universal-time)))

#|   hostname or ASN   |#
(defbind hostname "lambda.moo.mud.org")
#| port for connection |#
(defbind port 8888)

;; (defbind connection nil)                                                                                                                                                                                                                                                                                               

(defun connect-to-host ()
  "* and #' for: - connection - ."
  (defbind connection (open-telnet-session (hostname) (port))))

#|========================================================== |#


(defun format-to-connection (string)
  " to (connection) / *connection* "

(defun +init-doll+ ()
  (unwind-protect
       (unless (fbound (connect-to-host))
         (let*
             ((cons (doll/version) (cons (connection) . nil)))
           ((command (read-vailable-data (connection))))






(defun handle-stream-data ()
  " peek stream for data doll handles "
  ())


 #| super gross stuff soz |#
  #| not just imperative,  |#
   #| nor just hacky...     |#


(defun listener-loop-hacky ()
  (handle-stream-data)
  (read-available-data (connection))
  (sleep 1)
  (listener-loop-hacky))


(defun doll1 ()
  "see why this is mildly hacky at best etc"
  (make-thread #'listener-loop-hacky))

(defun doll2 ()
  " doll repl but all this needs writing "
  (let ((command (read-line)))
    (format-tn *connection* "~%~a~%" command))
  (doll2))
       
(defun doil ()
  (doll1)
  (doll2))
  
#| placeholder / notes code |#
       
(defthread listener-loop ()
  (read-available-data (connection))
  (listener-loop))
       (progn
         (when (stringp string)
           (format-tn (connection) "~a~%" string)))))

    (unwind-protect
       (progn
         (when (stringp string)
           (format-tn (connection) "~a~%" string)))))
