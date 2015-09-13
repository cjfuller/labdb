(in-package :cl-user)

(defpackage labdb.api
  (:use :cl :col :ningle :labdb.auth :labdb.config)
  (:export :*app*))

(in-package :labdb.api)

(cl-interpol:enable-interpol-syntax)

(defun json-reponse-middleware (app)
  (lambda (env)
    (destructuring-bind (status headers body) (funcall app env)
      (list status
            (mset headers :content-type "application/json;charset=utf-8"
                          :as-plist)
            body))))

(defparameter *api-app* (make-instance 'ningle:<app>))

(defparameter *app* (lack:builder
                     #'json-reponse-middleware
                     *api-app*))

(authroute *api-app* #?"${model-base}/:model/:idx" :method :GET
           (lambda (params)
             (->
              (labdb.models:get-json-by-id
               (intern (mget params :model) :keyword)
               (parse-integer (mget params :idx))))))

(authroute *api-app* #?"${model-base}/:model/:idx/next" :method :GET
           (lambda (params)
             (-> (<> #?"${api-base}${model-base}"
                     (labdb.models:get-next-uri
                      (mget params :model)
                      (parse-integer (mget params :idx))))
                 json:encode-json-to-string)))

(authroute *api-app* #?"${model-base}/:model/:idx/previous" :method :GET
           (lambda (params)
             (-> (<> #?"${api-base}${model-base}"
                     (labdb.models:get-previous-uri
                      (mget params :model)
                      (parse-integer (mget params :idx))))
                 json:encode-json-to-string)))

(cl-interpol:disable-interpol-syntax)
