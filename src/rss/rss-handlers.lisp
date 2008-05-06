(in-package :bknr.web)

;;; rss handlers
(defclass rss-handler (object-handler)
  ()
  (:default-initargs :query-function #'bknr.rss:find-rss-channel))

(defmethod handle-object ((handler rss-handler) (channel (eql nil)))
  (error "invalid channel name"))

(defmethod handle-object ((handler rss-handler) (channel bknr.rss:rss-channel))
  (with-http-response (:content-type "text/xml; charset=UTF-8")
    (with-output-to-string (stream)
      (bknr.rss:rss-channel-xml channel stream))))
