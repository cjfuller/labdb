(in-package :cl-user)

(defpackage :labdb.models
  (:use :cl :col :crane :split-sequence)
  (:export :db-test :get-by-id :get-json-by-id)
  ; TODO: fix shadowing by renaming table
  (:shadow :vector :sequence :search))

(in-package :labdb.models)

(enable-reader-exts)

(setup
 :migrations-directory
 (asdf:system-relative-pathname :labdb #P"migrations/")
 :databases
 `(:production
   (:type :postgres
    :name "labdb"
    :user "cfuller"
    :pass ,(uiop:getenv "POSTGRES_PW"))))

(connect)

;; Define a boolean type...

(crane.inflate-deflate:definflate (obj 'boolean) obj)
(crane.inflate-deflate:definflate (obj 'date) obj)
(crane.inflate-deflate:definflate (obj 'timestamp)
    (if (eql obj :null)
        nil
        obj))

(defmethod crane.inflate-deflate:inflate :around ((obj t) (type t))
  (call-next-method (if (eql obj :null) nil obj) type))

(defun model-data-hash (model-plist)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha1
    (flexi-streams:string-to-octets
     (json:encode-json-plist-to-string model-plist)
     :external-format :utf-8))))

(defun add-model-data-hash (model-plist)
  (mset model-plist :sha1 (model-data-hash model-plist)))

(defun fix-nulls (plist)
  (mapcar (lambda (item)
            (if (eql item :null)
                nil
                item))
          plist))

(defun ts->date (ts)
  (and ts
       (local-time:format-timestring
        nil
        (local-time:universal-to-timestamp ts)
        :format
        '(:year "-" (:month 2) "-" (:day 2)))))

(defun get-by-id (model-name id)
  (-> (query
         (select :*
                 (from model-name)
                 (where (:= :id id))))
      car
      fix-nulls
      add-model-data-hash))


(defun db-test ()
  (let ((q (fix-nulls (car (query
            (select :*
                    (from :plasmids)
                    (where (:= :id 1))))))))
    (cl-json:encode-json-plist-to-string q)))

(defun get-json-by-id (model-name id)
  (model->json
    (single
     (intern (string-upcase (symbol-name model-name)) :labdb.models)
     (:= :id id))))


(defclass entity () ())

;; Generic methods on labdb entities
(defgeneric name-abbrev (obj)
  (:documentation "Fetch the abbreviated name form for a model object (e.g. ASP)"))

(defgeneric number-field (obj)
  (:documentation "Fetch the name of the field that holds the object number."))

(defgeneric info-field (obj)
  (:documentation "Fetch the name of the field holding a short description."))

(defgeneric core-links (obj)
  (:documentation "Fetch a list of important linked items."))

(defgeneric designator (obj)
  (:documentation "Fetch the name + number designator for a model object (e.g. ASP792)"))

(defgeneric core-info-spec (obj)
  (:documentation "Fetch a plist of core info sections"))

(defgeneric sequence-info (obj)
  (:documentation "Fetch the sequence information, if any."))

(defgeneric supplemental-info-spec (obj)
  (:documentation "Fetch a plist of entries in the supplemental info"))

(defgeneric model->json (obj)
  (:documentation "Fetch the json representation of a model object."))

;; Entities

(defmethod designator ((obj entity))
  #?"${(name-abbrev obj)} ${(mget obj (number-field obj))}")

(defmethod core-links ((obj entity))
  nil)

(defmethod core-info-spec ((obj entity))
  nil)

(defmethod sequence-info ((obj entity))
  nil)

(defmethod supplemental-info-spec ((obj entity))
  nil)

(defun type-string (obj)
  (string-downcase (symbol-name (type-of obj))))

(defmethod model->json ((obj entity))
  (json:encode-json-plist-to-string
   (list
    :type (type-string obj)
    :resource-base (<> "/" (type-string obj))
    :name (designator obj)
    :short-desc (mget obj (info-field obj))
    :core-links (core-links obj)
    :core-info-sections (core-info-spec obj)
    :sequence-info (sequence-info obj)
    :supplemental-fields (supplemental-info-spec obj))))


;; Model definitions

(deftable plasmid (entity)
  (:deferredp t)
  (date_entered :type date :nullp t)
  (enteredby :type varchar :nullp t)
  (notebook :type varchar :nullp t)
  (verified :type boolean :nullp t)
  (plasmidalias :type varchar :nullp t)
  (antibiotic :type varchar :nullp t)
  (plasmidsize :type integer :nullp t)
  (concentration :type double :nullp t)
  (strainnumbers :type varchar :nullp t)
  (description :type text :nullp t)
  (sequence :type text :nullp t)
  (vector :type varchar :nullp t)
  (created_at :type timestamp :nullp t)
  (updated_at :type timestamp :nullp t)
  (plasmidnumber :type integer :nullp t)
  ;; legacy fields
  (plasmidmap_file_name :type varchar :nullp t)
  (plasmidmap_content_type :type varchar :nullp t)
  (plasmidmap_file_size :type varchar :nullp t)
  (plasmidmap_updated_at :type timestamp :nullp t))

(deftable antibody (entity)
  (:deferredp t)
  (ab_number :type integer :nullp t)
  (host :type varchar :nullp t)
  (label :type varchar :nullp t)
  (box :type varchar :nullp t)
  (alias :type varchar :nullp t)
  (fluorophore :type varchar :nullp t)
  (entered_by :type varchar :nullp t)
  (good_for_if :type boolean :nullp t)
  (good_for_western :type boolean :nullp t)
  (comments :type text :nullp t)
  (vendor :type varchar :nullp t)
  (created_at :type timestamp :nullp t)
  (updated_at :type timestamp :nullp t)
  (date_entered :type date :nullp t))

(deftable bacterialstrain (entity)
  (:deferredp t)
  (strain_number :type integer :nullp t)
  (species_bkg :type varchar :nullp t)
  (date_entered :type date :nullp t)
  (entered_by :type varchar :nullp t)
  (notebook :type varchar :nullp t)
  (genotype :type text :nullp t)
  (comments :type text :nullp t)
  (plasmid_number :type varchar :nullp t)
  (created_at :type timestamp :nullp t)
  (updated_at :type timestamp :nullp t)
  (sequence :type text :nullp t)
  (strainalias :type varchar :nullp t))

(deftable line (entity)
  (:deferredp t)
  (line_number :type integer :nullp t)
  (line_alias :type varchar :nullp t)
  (date_entered :type date :nullp t)
  (entered_by :type varchar :nullp t)
  (notebook :type varchar :nullp t)
  (species :type varchar :nullp t)
  (parent_line :type text :nullp t)
  (sequence :type text :nullp t)
  (description :type text :nullp t)
  (plasmid_numbers :type varchar :nullp t)
  (selectable_markers :type varchar :nullp t)
  (locations :type text :nullp t)
  (current_stock_counts :type text :nullp t)
  (created_at :type timestamp :nullp t)
  (updated_at :type timestamp :nullp t)
  (genotype :type text :nullp t)
  (stock_person :type text :nullp t)
  (stock_date :type text :nullp t))

(deftable oligo (entity)
  (:deferredp t)
  (oligo_number :type integer :nullp t)
  (oligoalias :type varchar :nullp t)
  (date_entered :type date :nullp t)
  (entered_by :type varchar :nullp t)
  (notebook :type varchar :nullp t)
  (vendor :type varchar :nullp t)
  (organism :type varchar :nullp t)
  (sequence :type text :nullp t)
  (purpose :type text :nullp t)
  (created_at :type timestamp :nullp t)
  (updated_at :type timestamp :nullp t))

(deftable sample (entity)
  (:deferredp t)
  (sample_number :type integer :nullp t)
  (sample_alias :type varchar :nullp t)
  (storage_type :type varchar :nullp t)
  (date_entered :type date :nullp t)
  (entered_by :type varchar :nullp t)
  (notebook :type varchar :nullp t)
  (sample_type :type varchar :nullp t)
  (depleted :type boolean :nullp t)
  (description :type text :nullp t)
  (linked_items :type text :nullp t)
  (created_at :type timestamp :nullp t)
  (updated_at :type timestamp :nullp t))

(deftable search (entity)
  (:deferredp t)
  (searchparams :type text :nullp t)
  (user_id :type integer :nullp t)
  (expires :type date :nullp t)
  (result :type text :nullp t)
  (created_at :type timestamp :nullp t)
  (updated_at :type timestamp :nullp t))

(deftable user (entity)
  (:deferredp t)
  (name :type varchar :nullp t)
  (created_at :type timestamp :nullp t)
  (updated_at :type timestamp :nullp t)
  (email :type varchar :nullp t)
  (auth_read :type boolean :nullp t)
  (auth_write :type boolean :nullp t)
  (auth_admin :type boolean :nullp t)
  (notes :type text :nullp t))

(deftable yeaststrain (entity)
  (:deferredp t)
  (strainalias :type varchar :nullp t)
  (antibiotic :type varchar :nullp t)
  (plasmidnumber :type varchar :nullp t)
  (strain_number :type integer :nullp t)
  (strain_bkg :type varchar :nullp t)
  (date_entered :type date :nullp t)
  (sequence :type text :nullp t)
  (entered_by :type varchar :nullp t)
  (comments :type text :nullp t)
  (genotype :type text :nullp t)
  (location :type varchar :nullp t)
  (species :type varchar :nullp t)
  (created_at :type timestamp :nullp t)
  (updated_at :type timestamp :nullp t)
  (notebook :type varchar :nullp t))


;; Plasmid methods

;; the table is named plasmids for historical reasons
(defmethod crane.meta:table-name ((cls (eql (find-class 'plasmid))))
  :plasmids)

;; TODO: make customizable
(defmethod name-abbrev ((obj plasmid))
  "ASP")

(defmethod number-field ((obj plasmid))
  'plasmidnumber)

(defmethod info-field ((obj plasmid))
  'plasmidalias)

(defvar bact-prefix "ASBS")

(defmethod core-links ((obj plasmid))
  (mapcar (lambda (link) (<> bact-prefix link))
          (split-sequence #\, (mget obj 'strainnumbers))))

(defmethod core-info-spec ((obj plasmid))
  (list
   (alist
    :name "Vector information"
    :fields (list
             (alist :name "Vector"
                    :value (vector obj))
             (alist :name "Antibiotic resistances"
                    :value (antibiotic obj))))
   (alist
    :name "Description"
    :preformatted t
    :inline-value (description obj))))

(defmethod sequence-info ((obj plasmid))
  (alist
   :sequence (sequence obj)
   :verified (verified obj)))

(defmethod supplemental-info-spec ((obj plasmid))
  (list
   (alist :name "Entered by"
          :value (enteredby obj))
   (alist :name "Date"
          :value (ts->date (date_entered obj)))
   (alist :name "Notebook"
          :value (notebook obj))
   (alist :name "Concentration"
          :value (concentration obj))))

;; Antibody methods

(defmethod crane.meta:table-name ((cls (eql (find-class 'antibody))))
  :antibodies)

;; TODO: make customizable
(defmethod name-abbrev ((obj antibody))
  "ASAB")

(defmethod number-field ((obj antibody))
  'ab_number)

(defmethod info-field ((obj antibody))
  'alias)

(defmethod core-info-spec ((obj antibody))
  (list
   (alist
    :name "Antibody information"
    :fields (list
             (alist :name "Host" :value (host obj))
             (alist :name "Fluorophores" :value (fluorophore obj))))
   (alist
    :name "Location information"
    :fields (list
             (alist :name "Box" :value (box obj))
             (alist :name "Label" :value (label obj))))
   (alist
    :name "Uses"
    :fields (list
             (alist :name "Good for IF" :value (good_for_if obj) :type :boolean)
             (alist :name "Good for westerns" :value (good_for_western obj) :type :boolean)))
   (alist
    :name "Description"
    :preformatted t
    :inline-value (comments obj))))

(defmethod supplemental-info-spec ((obj antibody))
  (list
   (alist :name "Entered by" :value (entered_by obj))
   (alist :name "Date" :value (date_entered obj))
   (alist :name "Vendor" :value (vendor obj))))

;; Bacterial strain methods
(defmethod crane.meta:table-name ((cls (eql (find-class 'bacterialstrain))))
  :bacteria)

(defmethod name-abbrev ((obj bacterialstrain))
  "ASBS")

(defmethod number-field ((obj bacterialstrain))
  'strain_number)

(defmethod info-field ((obj bacterialstrain))
  'strainalias)

;; TODO: get automatically
(defun plas-prefix ()
  "ASP")

(defmethod core-links ((obj bacterialstrain))
  (mapcar (lambda (link) (<> (plas-prefix) link))
          (split-sequence #\, (mget obj 'plasmid_number))))

(defmethod core-info-spec ((obj bacterialstrain))
  (list
   (alist :name "Strain information"
          :fields (list
                   (alist :name "Species and background" :value (species_bkg obj))
                   (alist :name "Genotype" :value (genotype obj))))
   (alist :name "Description"
          :preformatted t
          :inline-value (comments obj))))

(defmethod sequence-info ((obj bacterialstrain))
  (alist :sequence (sequence obj)
         :verified nil))

(defmethod supplemental-info-spec ((obj bacterialstrain))
  (list
   (alist :name "Entered by" :value (entered_by obj))
   (alist :name "Date" :value (date_entered obj))
   (alist :name "Notebook" :value (notebook obj))))

(defmethod crane.meta:table-name ((cls (eql (find-class 'line))))
  :lines)

(defmethod crane.meta:table-name ((cls (eql (find-class 'oligo))))
  :oligos)

(defmethod crane.meta:table-name ((cls (eql (find-class 'sample))))
  :samples)

(defmethod crane.meta:table-name ((cls (eql (find-class 'search))))
  :searches)

(defmethod crane.meta:table-name ((cls (eql (find-class 'user))))
  :users)

(defmethod crane.meta:table-name ((cls (eql (find-class 'yeaststrain))))
  :yeaststrains)

(setf (find-class 'plasmids) (find-class 'plasmid))
(setf (find-class 'antibodies) (find-class 'antibody))
(setf (find-class 'bacteria) (find-class 'bacterialstrain))
(setf (find-class 'lines) (find-class 'line))
(setf (find-class 'oligos) (find-class 'oligo))
(setf (find-class 'samples) (find-class 'sample))
(setf (find-class 'searches) (find-class 'search))
(setf (find-class 'users) (find-class 'user))
(setf (find-class 'yeaststrains) (find-class 'yeaststrain))

;; TODO: this chokes the first time we run it for each table that already exists.

(mapc #'crane:build
      '(plasmid antibody bacterialstrain line oligo sample search user yeaststrain))


(pop-reader-exts)
