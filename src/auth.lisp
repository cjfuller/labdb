(in-package :cl-user)

(defpackage labdb.auth
  (:use :cl :col :ningle)
  (:export :auth :with-auth-required :authroute))

(in-package :labdb.auth)

(defun auth (request)
  t)
  ;(equal (persona:persona request) "cjfuller@gmail.com"))

(defmacro with-auth-required (&body body)
  `(progn
     (unless (auth *request*)
       (error 'labdb.cond:http403
              :msg "Access denied."))
     ,@body))

(defmacro authroute (&rest args)
  `(setf (route ,@(butlast args))
         (lambda (params)
           (with-auth-required (funcall ,(car (last args)) params)))))
