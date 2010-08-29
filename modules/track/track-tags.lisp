(in-package :bknr-user)

(define-bknr-tag artist-url (artist)
  (format nil "/artist-tracks/~A" artist))

(define-bknr-tag genre-url (genre)
  (format nil "/genre-tracks/~A" (string-downcase (symbol-name genre))))

(define-bknr-tag artist-html-link (artist)
  (html ((:a :href (artist-url artist)) (:princ-safe artist))))

(define-bknr-tag genre-html-link (genre)
  (html ((:a :href (genre-url genre)) (:princ-safe (string-downcase (symbol-name genre))))))

(define-bknr-tag track-genre-choose-dialog (&key (size "4")
					       (name "genre")
					       (values nil)
					       (create nil))
  (let ((size (or (parse-integer size :junk-allowed t) 1)))
    (keyword-choose-dialog (all-track-genres)
			   :size size :name name :create create :values values)))

(define-bknr-tag track (&key id)
  (let ((track (find-store-object id :class 'track)))
    (when track
      (with-slots (name artist album genre) track
	(html ((:div :class "track")
	       (when name
		 (html ((:div :class "name") (:princ-safe name))))
	       (when artist
		 (html ((:div :class "artist") (artist-html-link artist))))
	       (when album
		 (html ((:div :class "album") (:princ-safe album))))
	       (when genre
		 (html ((:div :class "genre") (dolist (g genre)
						(html " " (genre-html-link g) " ")))))))))))

(defun track-edit-form (&key name artist album genre (create t) (size "4"))
  (html (:tr (:td "name")
	     (:td (text-field "name" :value name)))
	(:tr (:td "artist")
	     (:td (text-field "artist" :value artist)))
	(:tr (:td "album")
	     (:td (text-field "album" :value album)))
	(:tr (:td "genres")
	     (:td (track-genre-choose-dialog :name "genre"
					   :create create :size size
					   :values (when genre (list genre)))))))

(define-bknr-tag track-form (&key id)
  (let ((track (find-store-object id :class 'track)))
    (when track
      (with-slots (name artist album genre) track
	(html ((:table :class "track")
	       ((:form :method "POST")
		(track-edit-form :name name :artist artist :album album :genre (first genre))
		(:tr ((:td :colspan 2)
		      (submit-button "save" "save")
		      (submit-button "delete" "delete"))))))))))

(define-bknr-tag search-track (&key title)
  (declare (ignore title))
  (with-query-params (name artist album genre)
    (let ((genre (when genre (make-keyword-from-string genre))))
      (html ((:form :method "POST")
	     ((:table :class "search-panel")
	      (track-edit-form :name name :artist artist :album album :genre genre
			       :create nil :size "1")
	      (:tr (:td "operator")
		   (:td (keyword-choose-dialog '(:or :and)
					       :size 1 :name "operator" :values '(:or))))
	      (:tr ((:td :colspan 2)
		    (submit-button "search" "search")))))))))

(define-bknr-tag edit-track-collection (&key title
					   (tracks (session-value :current-track-result)))
  (when tracks
    (html ((:div :class "edit-tracks")
	   ((:form :method "POST")
	    (when title
	      (html (:h2 (:princ-safe title))))
	    ((:table)
	     (:tr ((:td :colspan 2) (:h3 "Edit selected tracks:")))
	     (track-edit-form)
	     (:tr ((:td :colspan 2)
		   (submit-button "add-genres" "add-genres")
		   (submit-button "remove-genres" "remove-genres")
		   (submit-button "save-all" "save-all")
		   (submit-button "delete-all" "delete-all"))))
	    (:table
	     (loop for track-row in (group-by tracks 3)
		   do (html
		       (:tr
			(loop for track in track-row
			      do (html ((:td :class "edit-track")
					(track :id (store-object-id track))
					((:div :align "center")
					 ((:input :type "checkbox"
						  :name "track-id"
						  :value (store-object-id track)
						  :checked "checked")))))))))))))))