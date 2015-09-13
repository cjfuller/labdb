(in-package :cl-user)
(defpackage :labdb.config
  (:use :cl :col)
  (:export :api-base :model-base))

(in-package :labdb.config)

(defvar api-base "/api/v1")

(defvar model-base "/m")
