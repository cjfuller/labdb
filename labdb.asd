(asdf:defsystem labdb
  :description "Laboratory database system."
  :version "2.0.0.pre1"
  :license "AGPLv3"
  :depends-on
  (:col :ningle :clack :ningle :lack :hunchentoot
   :drakma :cl-json :cl+ssl :flexi-streams :bordeaux-threads
   :crane :lack-middleware-static :ironclad :split-sequence
   :lack-middleware-session :lack-middleware-accesslog :djula :local-time)
  :components
  ((:module "src"
    :components ((:file "main"
                  :depends-on ("persona" "errors" "auth" "api" "models" "config"))
                 (:file "errors")
                 (:file "config")
                 (:file "auth")
                 (:file "models"
                  :depends-on ("config"))
                 (:file "api"
                  :depends-on ("auth" "errors" "models" "config"))
                 (:file "persona")))))
