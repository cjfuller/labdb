(in-package :cl-user)

(defpackage labdb.api
  (:use :cl :col :ningle :labdb.auth)
  (:export :*app*))

(in-package :labdb.api)

(defparameter *app* (make-instance 'ningle:<app>))

(setf (route *app* "/testing" :method :GET)
      (lambda (params)
        (labdb.models:db-test)))

(authroute *app* "/m/:modeltype/:idx" :method :GET
           (lambda (params)
             (->
              (labdb.models:get-json-by-id
               (intern (mget params :modeltype) :keyword)
               (parse-integer (mget params :idx))))))

