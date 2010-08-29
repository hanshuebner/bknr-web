(in-package :bknr.mail)

(enable-interpol-syntax)

(defun unquote (string)
  (if (= 2 (length string))
      nil
      (subseq string 1 (- (length string) 1))))

(defun parse-dirty-integer (string)
  (parse-integer string :junk-allowed t))

(defun parse-yahoo-date (date-string)
  "parse yahoo date (MM/DD/YYYY) to lisp universal time"
  (destructuring-bind (month date year &rest junk)
      (mapcar #'parse-dirty-integer (split "/" date-string))
    (declare (ignore junk))
    (encode-universal-time 0 0 0 date month year)))

(defmethod import-yahoo-groups-to-ml ((mailinglist mailinglist) filename)
  (let (line)
    (handler-case
	(with-open-file (f filename)
	  (loop for line = (read-line f nil nil)
             while line
             unless (equal "" line)
             do  (let* ((fields (mapcar #'unquote (split "(?<='),(?=')" line)))
                        (yahoo-name (nth 0 fields))
                        (email (nth 1 fields))
                        (name (nth 5 fields))
                        (subscription-type (nth 6 fields))
                        (subscription-started (parse-yahoo-date (nth 8 fields))))
                   (mailinglist-subscribe-user mailinglist
                                               (or (find-user (or yahoo-name email))
                                                   (make-instance 'user :login (or yahoo-name email)
                                                                        :email email
                                                                        :full-name name))
                                               :type (if (scan "Digest" subscription-type) :digest :mail)
                                               :started subscription-started))))
      (error (e)
	(warn "can't process yahoo groups export line ~a: ~a" line e)))))