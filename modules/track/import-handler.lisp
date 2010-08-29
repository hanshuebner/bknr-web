(in-package :bknr-user)

(enable-interpol-syntax)

(defclass mp3-import-handler (import-handler)
  ())

(defmethod import-handler-spool-files ((handler mp3-import-handler))
  (directory (merge-pathnames "**/*.mp3" (import-handler-import-pathname handler))))

(defmethod authorized-p ((handler mp3-import-handler))
  (or (admin-p *user*)
      (user-has-flag *user*  :music)))

(defmethod handle-form ((handler mp3-import-handler) action)
  (with-bknr-page (:title #?"mp3 import directory")
    ((:form :method "post")
     ((:div :class "keyword-choose" :align "center")
      (:h2 "Choose tags for the imported mp3s (nothing will read them from the ID3 tags):")
      (:table (:tr (:td "genre")
		   (:td (track-genre-choose-dialog
			 :name "genre-str" :create t :size "4")))
	      (:tr (:td "artist")
		   (:td (text-field "artist")))
	      (:tr (:td "album")
		   (:td (text-field "album"))))
      (:div "Set tags from directory: "
	    (select-box "tagsfromdir"
				       '(("genre/artist/album" :genre-artist-album)
					 ("artist/album" :artist-album)
					 ("artist" :artist))
				       :default nil))
      (:div (submit-button "import" "Import"))))
    ((:div :class "import-list")
     (:h2 "Mp3s present in import spool:")
     (loop for file in (import-handler-spool-files handler)
	   do (html (:princ-safe (namestring file)) (:br))))))


(defmethod import-handler-import-files ((handler mp3-import-handler))
  (let ((genre (keywords-from-query-param-list (query-param-list "genre"))))
    (with-query-params (artist album tagsfromdir)
      (let ((tags-from-dir (case tagsfromdir
			     (:genre-artist-album '(:genre :artist :album))
			     (:artist-album '(:artist :album))
			     (:artist '(:artist))
			     (t nil)))
	    (spool-dir (import-handler-import-pathname handler)))
	(import-mp3-directory :spool spool-dir
			      :genre genre :album album :artist artist
			      :tags-from-dir tags-from-dir)))))

(defmethod handle-form ((handler mp3-import-handler) (action (eql :import)))
  (let* ((import-log (import-handler-import-files handler))
	 (successful-tracks (remove-if-not #'(lambda (element) (typep element 'track))
					   import-log
					   :key #'cdr))
	 (error-log (remove-if-not #'(lambda (element) (typep element 'error))
				   import-log
				   :key #'cdr)))
    (with-bknr-page (:title #?"bknr import log")
      ((:div :class "error-log") (:h2 "Errors during import:")
       (loop for (file . error) in error-log
	     do (typecase error
		  (index-existing-error
		   (html "Error importing " (:princ-safe file)
			 " because it already is present in the system" (:br)))
		  (t
		   (html "Error importing " (:princ-safe file)
			 (:princ-safe (format nil ": ~a" error)) (:br))))))
      (:h2 "Successfully imported mp3s:")
      (:ul (dolist (track (mapcar #'cdr successful-tracks))
	     (html (:li (html-link track))))))))
