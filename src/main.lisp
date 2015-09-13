(in-package :cl-user)

(defpackage labdb.main
  (:use :cl :col :ningle :labdb.auth :labdb.config)
  (:export :*app* :*app-with-middleware* :start :reload))

(in-package :labdb.main)

(enable-reader-exts)

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
               (render-template
                "index.html"
                :content-json
                item-json))))

(authroute *app* "/:model" :method :GET
           (lambda (params)
             (let ((items
                     (-> (mget params :model)
                         (labdb.models:get-partial-list :limit 100 :start 0)
                         (rmapcar #'plist->alist))))
               (render-template
                "index.html"
                :content-json
                (json:encode-json-plist-to-string
                 (list
                  :type :collection
                  :items
                  items
                  :start 0
                  :end (+ 0 (length items))
                  :count
                  (labdb.models:item-count (mget params :model))
                  :resource #?"/${(mget params :model)}"))))))

(authroute *app* "/bacteria"
           (lambda (params)
             (forward params)))

(setf (route *app* "/.*" :regexp t)
      #'forward)

(defparameter *app-with-middleware*
  (lack:builder
   :accesslog
   :session
   (:static :path "/_s/" :root (asdf:system-relative-pathname :labdb #P"public/"))
   #'labdb.cond:http-condition-middleware
   (:mount api-base labdb.api:*app*)
   *app*))
