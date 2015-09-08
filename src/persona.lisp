(in-package :cl-user)

(defpackage :persona
  (:use :cl)
  (:export
   :verify-assertion
   :handler
   :persona))

(in-package :persona)

(defvar *default-verifier-uri* "https://verifier.login.persona.org/verify")

(defun verify-assertion (assertion audience
                         &optional (verifier-uri *default-verifier-uri*))
  (multiple-value-bind
   (body status headers uri stream close-p reason)
   (drakma:http-request
    verifier-uri
    :method :POST
    :force-ssl t
    :verify :required
    :form-data nil
    :parameters `(("assertion" . ,assertion)
                  ("audience" . ,audience)))
    (if (eql status 200)
        (let ((parsed (json:decode-json-from-string
                       (flexi-streams:octets-to-string body :external-format :utf-8))))
          (and (equal "okay" (cdr (assoc "status" parsed :test #'equal)))
               (cdr (assoc "email" parsed :test #'equal))))
        nil)))

(defun handler (params request)
  (let ((assertion (cdr (assoc "assertion" params :test #'equal)))
        ; TODO: match protocol
        ; TODO: figure out why the verifier isn't working.
        (audience (format nil "http://~A:~A~A"
                          (lack.request:request-server-name request)
                          (lack.request:request-server-port request)
                          (lack.request:request-uri request))))
    (let ((verifier-response (verify-assertion assertion audience)))
      (let ((session (getf (lack.request:request-env request) :lack.session)))
        (if verifier-response
            (setf (gethash :persona session) verifier-response)
            (remhash :persona session))))))

(defun persona (request)
  (gethash :persona (getf (lack.request:request-env request) :lack.session)))

(defun persona-non-grata (request)
  (remhash :persona (getf (lack.request:request-env request) :lack.session)))
