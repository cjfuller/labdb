(in-package :cl-user)

(defpackage labdb.main
  (:use :cl :col :ningle :labdb.auth)
  (:export :*app* :start :reload))

(in-package :labdb.main)

(defparameter *app* (make-instance 'ningle:<app>))
(defvar handler nil)

(djula:add-template-directory
 (asdf:system-relative-pathname :labdb "templates/"))

(defun render-template (fn &rest args)
  (let ((s (make-string-output-stream)))
    (apply
     #'djula:render-template*
     (djula:compile-template* fn)
     s
     args)
    (get-output-stream-string s)))

(defun forward (params)
  (declare (ignore params))
  (when (cl-ppcre:scan "\\.css" (lack.request:request-uri *request*))
    (setf (lack.response:response-headers *response*)
          (mset (lack.response:response-headers *response*)
                :content-type
                "text/css"
                :as-plist)))
  (drakma:http-request
   (<> "https://localhost:3000" (lack.request:request-uri *request*))
   :method (lack.request:request-method *request*)))

(setf (route *app* "/_i/persona/verify_assertion" :method :POST)
      (lambda (params) (persona:handler params *request*)))

(setf (route *app* "/assets/*" :method :GET)
      (lambda (params) (forward params)))

(authroute *app* "/:model/:idx" :method :GET
           (lambda (params)
             (let ((item-json
                     (labdb.models:get-json-by-id (intern (mget params :model) :keyword)
                                                  (parse-integer (mget params :idx)))))
               (println item-json)
               (render-template
                "object_display.html"
                :item-json
                item-json
                ))))

(authroute *app* "/:model" :method :GET
           (lambda (params)
             (println (assoc "model" params :test #'equal))
             (forward params)))

(authroute *app* "/bacteria"
           (lambda (params)
             (forward params)))

(setf (route *app* "/.*" :regexp t)
      #'forward)

(defun start ()
  (setf
   handler
   (let ((*app*
           (lack:builder
            :accesslog
            :session
            (:static :path "/_s/" :root (asdf:system-relative-pathname :labdb #P"public/"))
            #'labdb.cond:http-condition-middleware
            (:mount "/api/v1" labdb.api:*app*)
            *app*)))
     (clack:clackup *app* :silent nil :use-thread nil))))

(defun reload ()
  (clack:stop handler)
  (crane:disconnect)
  (ql:quickload :labdb)
  (start))
