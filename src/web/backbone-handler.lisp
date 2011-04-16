#|

 backbone.js handler implementation for BKNR

 (c) Apr 2011 - Manuel Odendahl - wesen@ruinwesen.com


 This handler provides a CRUD endpoint for backbone.js operations on a BKNR datastore class.
|#

(in-package :bknr.web)

(defclass backbone-handler (object-handler)
  ())

(defgeneric backbone-model-id (handler)
  (:documentation "Get the id of the currently request model by parsing the SCRIPT-NAME*."))

(defmethod backbone-model-id ((handler backbone-handler))
  (values-list (last (mapcar #'url-decode (split "/" (handler-path handler))))))

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
        (bknr.datastore:*json-ignore-slots* '(bknr.indices::destroyed-p)))
    (with-output-to-string (stream)
      (case (hunchentoot:request-method*)
        (:GET
         (if id
             (yason:encode (object-handler-get-object handler) stream)
             (yason:encode (store-objects-with-class (object-handler-object-class handler)) stream)))
        (:POST
         (let ((msg (yason:parse (hunchentoot:raw-post-data :force-text t))))
           (yason:encode msg stream)))
        (:PUT
         (yason:encode (alexandria:plist-hash-table '()) stream))
        (:DELETE "")))))