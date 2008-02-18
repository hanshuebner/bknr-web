
(in-package :bknr.web)

(enable-interpol-syntax)

(define-persistent-class host ()
  ((name :update :initform nil)
   (ip-address :update
	       :index-type string-unique-index
	       :index-reader host-with-ipaddress
	       :index-keys all-hosts)
   (last-seen :update :initform 0))
  (:default-initargs :ip-address "")
  (:documentation "ip host which has visited the system through http or another protocol"))

(defmethod print-object ((host host) stream)
  (format stream "#<~a ~a [~a]>" (class-name (class-of host))
	  (host-name host) (host-ip-address host))
  host)

#+(or)
(defun resolve-ip-address (address)
  ;; this uses the resolver library which still depends on libresolv.so and thus is blocking.
  (let ((hostname (first (last (first
				(resolver:get-answers
				 (resolver:lookup (format nil "~{~a.~}in-addr.arpa."
							  (reverse (split #?r"\." address)))
						  'ptr)))))))
    (if hostname
	(regex-replace #?r"\.$" hostname "")
	nil)))

(defmethod resolve-ipaddr ((host host))
  (unless (slot-value host 'name)
    (change-slot-values host 'name
			(or #+(or)
			    ;; for now, don't do hostname lookups
			    (ignore-errors (socket:ipaddr-to-hostname
					    (socket:dotted-to-ipaddr (host-ip-address host))))
			    ""))))

(defmethod host-name ((host host))
  (resolve-ipaddr host)
  (if (not (equal (slot-value host 'name) ""))
      (slot-value host 'name)
    (host-ip-address host)))

(defmethod host-ipaddr ((host host))
  (usocket:host-byte-order (host-ip-address host)))

(defun find-host (&key ip-address create ipaddr)
  (when ipaddr
    (setf ip-address (usocket:hbo-to-dotted-quad ipaddr)))
  (or (host-with-ipaddress ip-address)
      (and create
	   (make-object 'host :ip-address ip-address))))

