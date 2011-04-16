#|

 backbone.js handler implementation for BKNR

 (c) Apr 2011 - Manuel Odendahl - wesen@ruinwesen.com


 This handler provides a CRUD endpoint for backbone.js operations on a BKNR datastore class.
|#

(in-package :bknr.web)

(defclass backbone-handler (object-handler)
  ((slot-whitelist :initarg :slot-whitelist
                   :initform '()
                   :reader backbone-handler-slot-whitelist
                   :type list
                   :documentation "List of slots that are allowed to be updated by the handler, as symbol list")
   (slot-blacklist :initarg :slot-blacklist
                   :initform '(bknr.datastore::id
                               bknr.datastore::last-change)
                   :reader backbone-handler-slot-blacklist
                   :type list
                   :documentation "List of slots that are forbidden to be updated by the handler, as symbol list")))

;;; generic methods for backbone handler

(defgeneric backbone-model-id (handler)
  (:documentation "Get the id of the currently request model by parsing the SCRIPT-NAME*."))


(defgeneric json-update-object (handler obj alist)
  (:documentation "Update an object using a parsed json message as ALIST"))

(defgeneric json-create-object (handler alist)
  (:documentation "Create an object using a parsed json message as ALIST"))

(defgeneric backbone-handler-parse-json (handler json)
  (:documentation "Generic methods transforming the received json (as raw text) into an update alist to be passed to JSON-UPDATE-OBJECT."))

;;; concrete methods for backbone handler

(defun symbol-to-json (symbol)
  "Helper to convert a LISP symbol to a JSON string"
  (string-downcase (symbol-name symbol)))

(defmethod backbone-model-id ((handler backbone-handler))
  (values-list (last (mapcar #'url-decode (split "/" (handler-path handler))))))

(defmethod backbone-handler-parse-json ((handler backbone-handler) json)
  "Parse the JSON into an alist, removing blacklisted slots and slots not whitelisted."
  (let ((alist (alexandria:hash-table-alist (json:parse json)))
        (blacklist (mapcar #'symbol-to-json (backbone-handler-slot-blacklist handler)))
        (whitelist (mapcar #'symbol-to-json (backbone-handler-slot-whitelist handler))))
    (remove-if #'(lambda (x)
                   (or (member x blacklist :test #'string-equal)
                       (not (member x whitelist :test #'string-equal))))
               alist :key #'car)))

(defmethod json-update-object ((handler backbone-handler) (obj store-object) alist)
  "Update a persistent object by grouping together all the slot updates for allowed slots into a transaction."
  (let* ((slotdefs (closer-mop:class-slots (class-of obj)))
         ;; get an alist from json name to slot definition name
         (json2name (mapcar #'(lambda (slotdef)
                                (cons (symbol-to-json (closer-mop:slot-definition-name slotdef))
                                      (closer-mop:slot-definition-name slotdef)))
                            slotdefs)))

    ;; got over the json update object
    (with-transaction ()
      (dolist (kv alist)
        (let* ((key (car kv))
               (value (cdr kv))
               (slot-name (cdr (assoc key json2name :test #'string-equal))))
          (when slot-name
            (format t "updating slot ~S with value ~S~%" slot-name value)
            (setf (slot-value obj slot-name) value)))))))

(defmethod json-create-object ((handler backbone-handler) alist)
  "Create a new store object by looking up the initargs for the allowed slots and calling MAKE-INSTANCE."
  (let* ((object-class (find-class (object-handler-object-class handler)))
         (slotdefs (closer-mop:class-slots object-class))
         (json2initargs (mapcar #'(lambda (slotdef)
                                   (cons (symbol-to-json (closer-mop:slot-definition-name slotdef))
                                         (closer-mop:slot-definition-initargs slotdef)))
                                slotdefs)))
    (let ((initform (mapcan #'(lambda (kv)
                                (let* ((key (car kv))
                                       (value (cdr kv))
                                       (initargs (cdr (assoc key json2initargs :test #'string-equal))))
                                  (when initargs
                                    (list (first initargs) value))))
                            alist)))
      (apply #'make-instance (object-handler-object-class handler) initform))))
           
(defmethod object-handler-get-object ((handler backbone-handler))
  "Get the object by getting the id from the backbone handler url."
  (let ((id (backbone-model-id handler)))
    (when id
      (find-store-object id
                         :class (object-handler-object-class handler)
                         :query-function (object-handler-query-function handler)))))

(defmethod handle ((handler backbone-handler))
  "Handle CRUD updates for backbone collections and objects.

  GET /prefix/ID   -> return a single object as JSON
  GET /prefix      -> return all the class objects as JSON

  POST /prefix     -> create a new object

  PUT  /prefix/id  -> update an existing object

  DELETE /prefix/id -> delete an existing object
  "
  (let ((id (backbone-model-id handler))
        (bknr.datastore:*json-ignore-slots* '(bknr.indices::destroyed-p))
        (obj (object-handler-get-object handler)))
    (with-output-to-string (stream)
      (case (hunchentoot:request-method*)

        (:GET
         (if id
             (yason:encode obj stream)
             (yason:encode (store-objects-with-class (object-handler-object-class handler)) stream)))

        (:POST
         (let ((obj (json-create-object handler
                                        (backbone-handler-parse-json handler (hunchentoot:raw-post-data :force-text t)))))
           (yason:encode obj stream)))
        
        (:PUT
         (when obj
           (json-update-object handler obj
                               (backbone-handler-parse-json handler (hunchentoot:raw-post-data :force-text t)))
           (yason:encode obj stream)))

        (:DELETE
         (when obj
           (destroy-object obj)))))))