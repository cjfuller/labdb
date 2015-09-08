(in-package :cl-user)

(defpackage :labdb.cond
  (:use :cl)
  (:export
   :http-status
   :http4xx
   :http400
   :http401
   :http403
   :http-condition-middleware))

(in-package :labdb.cond)

(define-condition http-status (simple-error)
  ((status :allocation :class :initarg :status)
   (spec-name :allocation :class :initarg :spec-name)
   (msg :initarg :msg :initform "")))

(define-condition http4xx (http-status) ())

(define-condition http400 (http4xx)
  ((status :allocation :class :initform 400)
   (spec-name :allocation :class :initform "Bad Request")))

(define-condition http401 (http4xx)
  ((status :allocation :class :initform 401)
   (spec-name :allocation :class :initform "Unauthorized")))

(define-condition http403 (http4xx)
  ((status :allocation :class :initform 403)
   (spec-name :allocation :class :initform "Forbidden")))

(defun http-condition-middleware (app)
  (lambda (env)
    (handler-case
        (funcall app env)
      (http-status (err)
        (with-slots (status spec-name msg) err
            `(,status (:content-type "text/plain; charset=utf-8") (,(format nil "~A: ~A" spec-name msg))))))))

