(in-package :bknr.rss)

;; RSS 2.0 Generation Package

;; This package aids in the automatic generation of RSS channels.

;; See the documentation to class RSS-CHANNEL for an overview.

;;; Paul Graham, On Lisp, p191
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

;; Class for channels

(define-persistent-class rss-channel ()
  ((name :update
	 :index-type string-unique-index
	 :index-reader find-rss-channel)
   (title :update :initform nil)
   (path :update :initform nil)
   (description :update :initform nil)
   (last-update :update :initform (get-universal-time))
   (max-item-age :update
                 :initform 28
                 :documentation "default maximum item age in days")
   (items :none :initform nil))
  (:documentation "RSS-CHANNEL models one rss channel.  Items are
added to a channel by deriving other persistent classes from the mixin
class RSS-ITEM.  When an object of such a derived class is created, it
is automatically added to its RSS channel.  Likewise, it is
automatically deleted from the channel when it is deleted.

The channel that an item is put into is defined by the generic
function RSS-ITEM-CHANNEL which needs to be specialized for each item
class.  The default method of this generic function specifies nil as
channel, which results in the creation of a warning message when an
object of this class is created.

The RSS-ITEM-CHANNEL method may return the channel either as a string
or as a channel object.

Subclasses of RSS-ITEM should provide methods for some of the generic
functions (RSS-ITEM-CHANNEL RSS-ITEM-TITLE RSS-ITEM-LINK
RSS-ITEM-DESCRIPTION RSS-ITEM-AUTHOR RSS-ITEM-CATEGORY
RSS-ITEM-COMMENTS RSS-ITEM-ENCLOSURE RSS-ITEM-GUID RSS-ITEM-SOURCE).
These functions are called when the RSS file for the channel is
generated and provide the content in the RSS items.

One RSS-ITEM can only be in one channel, which is a restriction that
may eventually be removed.

The channel object has more required elements than specified by the
standard in order to make the generated feed documents more widely
accepted."))

(defmethod print-object ((channel rss-channel) stream)
  (print-unreadable-object (channel stream :type t :identity t)
    (format stream "~A (~A item~:P)"
            (rss-channel-name channel)
            (length (rss-channel-items channel)))))

(defmethod prepare-for-snapshot ((channel rss-channel))
  "When snapshotting, remove items from CHANNEL that are destroyed."
  (setf (slot-value channel 'items)
        (remove-if #'object-destroyed-p (slot-value channel 'items))))

;; Mixin for items

(define-persistent-class rss-item ()
  ()
  (:documentation "Mixin class for RSS items.  See documentation for
class RSS-CHANNEL for an overview."))

(defun make-rss-channel (name title description path &rest args)
  "Create an RSS channel with the given NAME, TITLE, DESCRIPTION and
LINK (all strings) which are the mandatory fields in an RSS channel.
Returns the persistent RSS-CHANNEL object that has been created."
  (apply #'make-instance 'rss-channel :name name :title title :description description :path path args))
  
(defun render-mandatory-element (channel element)
  (with-element (string-downcase (symbol-name element))
    (let ((value (funcall (find-symbol (format nil "RSS-CHANNEL-~A" element) :bknr.rss) channel)))
      (text (or value (format nil "(channel ~(~A~) not defined)" element))))))

(defmethod rss-channel-link ((channel rss-channel))
  (format nil "http://~A/~A" (bknr.web:website-host) (rss-channel-path channel)))

(defgeneric rss-channel-xml (channel stream)
  (:documentation "Generate XML for the current state of RSS channel
CHANNEL to STREAM.")
  (:method ((channel rss-channel) stream)
    (with-xml-output (make-character-stream-sink stream)
      (with-element "rss"
        (attribute "version" "2.0")
        (attribute* "xmlns" "content" "http://purl.org/rss/1.0/modules/content/")
        (attribute* "xmlns" "atom" "http://www.w3.org/2005/Atom")
        (with-element "channel"
          (render-mandatory-element channel 'title)
          (render-mandatory-element channel 'description)
          (with-element "link"
            (text (rss-channel-link channel)))
          (with-element* ("atom" "link")
            (attribute "href" (rss-channel-link channel))
            (attribute "rel" "self")
            (attribute "type" "application/rss+xml"))
	
          (dolist (item (remove-if-not #'(lambda (item)
                                           (and (not (object-destroyed-p item))
                                                (rss-item-published item)))
                                       (rss-channel-items channel)))
            (rss-item-xml item)))))))

(defun days-from-query-parameter ()
  (when (boundp 'hunchentoot:*request*)
    (let ((days-string (bknr.web:query-param "days")))
      (when days-string
        (parse-integer days-string)))))

(defun month-from-query-parameter ()
  (when (boundp 'hunchentoot:*request*)
    (let ((month-string (bknr.web:query-param "month")))
      (when month-string
        (mapcar #'parse-integer (cl-ppcre:split "([-/]|(?<=....))" month-string :limit 2))))))

(defun rss-channel-archive (channel)
  "Return the channel archive consisting of lists of lists ((MONTH YEAR) ITEM...)"
  (group-on (rss-channel-items channel :all t)
            :test #'equal
            :key (lambda (item)
                   (multiple-value-bind (seconds minutes hours day month year)
                       (decode-universal-time (rss-item-pub-date item))
                     (declare (ignore seconds minutes hours day))
                     (list year month)))))

(defgeneric rss-channel-items (channel &key)
  (:documentation "Return all non-expired items in channel.")
  (:method ((channel rss-channel) &key days month count all)
    (unless month
      (setf month (month-from-query-parameter)))
    (unless days
      (setf days (or (days-from-query-parameter)
                     (rss-channel-max-item-age channel))))
    (if all
        (remove-if #'object-destroyed-p (slot-value channel 'items))
        (let ((items (if month
                         (cdr (find month (rss-channel-archive channel) :test #'equal :key #'car))
                         (let ((expiry-time (- (get-universal-time) (* 60 60 24 days))))
                           (remove-if (lambda (item) (or (object-destroyed-p item)
                                                         (< (rss-item-pub-date item) expiry-time)))
                                      (slot-value channel 'items))))))
          (if count
              (subseq items 0 (min count (length items)))
              items)))))

(defgeneric rss-channel-archived-months (channel)
  (:documentation "Return a list of lists (MONTH YEAR) for which the
  CHANNEL has archived entries.")
  (:method (channel)
    (mapcar #'car (rss-channel-archive channel))))
  
(deftransaction rss-channel-cleanup (channel)
  "Remove expired items from the items list.  Can be used to reduce
the memory footprint of very high volume channels."
  (setf (slot-value channel 'items) (rss-channel-items channel)))

(defgeneric remove-item (channel item)
  (:documentation "Remove ITEM from CHANNEL.  May only be called
within transaction context.")
  (:method ((channel rss-channel) item)
    (setf (slot-value channel 'items) (remove item (slot-value channel 'items))))
  (:method ((channel string) item)
    (aif (find-rss-channel channel)
         (remove-item it item)))
  (:method ((channel (eql nil)) item)
    (warn "no RSS channel defined for item ~A" item)))

(defgeneric add-item (channel item)
  (:documentation "Add ITEM to CHANNEL.  May only be called within
transaction context.")
  (:method ((channel rss-channel) item)
    (push item (slot-value channel 'items)))
  (:method ((channel string) item)
    (aif (find-rss-channel channel)
         (add-item it item)
         (warn "can't find RSS channel ~A to add newly created item ~A to" channel item)))
  (:method ((channel (eql nil)) item)
    (warn "no RSS channel defined for item ~A" item)))

(defmethod initialize-instance :after ((rss-item rss-item) &key)
  (add-item (rss-item-channel rss-item) rss-item))

(defmethod destroy-object :before ((rss-item rss-item))
  (remove-item (rss-item-channel rss-item) rss-item))

(defun item-slot-element (item slot-name)
  "Cheapo helper function to map from a pseudo slot name to an accessor."
  (let ((accessor (find-symbol (format nil "RSS-ITEM-~A" slot-name) (find-package :bknr.rss))))
    (aif (funcall accessor item)
	 (with-element (string-downcase (symbol-name slot-name))
	   (text it)))))

(defun rss-item-xml (item)
  "Generate RSS XML for ITEM using CXML's unparse functionality."
  (with-element "item"
    (dolist (slot '(title link author category comments enclosure source))
      (item-slot-element item slot))
    (aif (rss-item-guid item)
	 (with-element "guid"
	   (attribute "isPermaLink" "true")
	   (text it)))
    (aif (rss-item-description item)
	 (with-element "description"
	   (cdata it)))
    (with-element "pubDate"
      (text (format-date-time (rss-item-pub-date item) :mail-style t)))
    (aif (rss-item-encoded-content item)
	 (with-element* ("content" "encoded" )
	   (cdata it)))))

;; All items present on an RSS stream can implement the access
;; methods below.

(defgeneric rss-item-pub-date (item)
  (:documentation "The default implementation for the publication date
delivers the current system date/time as publication date.")
  (:method (item)
    (warn "no rss-item-pub-date defined for class ~A, using current date/time" (class-of item))
    (get-universal-time)))

(defgeneric rss-item-published (item)
  (:documentation "Return non-nil if the ITEM is published.
Non-published items are not put into generated XML by
RSS-CHANNEL-XML.")
  (:method (item)
    t))

(defmacro define-rss-item-field (field-name
                                 &key
                                 (documentation (format nil "Return the ~(~A~) of the ITEM as a string" field-name))
                                 mandatory)
  `(defgeneric ,(intern (format nil "RSS-ITEM-~A" field-name)) (item)
     (:documentation ,(format nil "~A~@[ (optional)~]"
                              documentation (not mandatory)))
     ,@(unless mandatory
               '((:method (item) nil)))))

(define-rss-item-field channel
    :documentation "Return the channel that the ITEM is published in."
    :mandatory t)
(define-rss-item-field title)
(define-rss-item-field link)
(define-rss-item-field description)
(define-rss-item-field author)
(define-rss-item-field category)
(define-rss-item-field comments)
(define-rss-item-field enclosure)
(define-rss-item-field guid)
(define-rss-item-field source)
(define-rss-item-field encoded-content
    :documentation "Return the content for ITEM in encoded (usually HTML) form as string.")
