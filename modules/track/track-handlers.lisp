(in-package :bknr-user)

(enable-interpol-syntax)

(defmethod edit-object-url ((track track))
  (format nil "/edit-track/~A"
	  (store-object-id track)))

(defmethod object-url ((track track))
  (format nil "/track/~A"
	  (store-object-id track)))

(defmethod html-link ((track track))
  (html ((:a :href (object-url track))
	 (:princ-safe (track-name track)))))

(defmethod html-edit-link ((track track))
  (html ((:a :href (edit-object-url track))
	 "edit " (:princ-safe (track-name track)))))

(defclass track-handler (object-handler)
  ())

(defmethod object-handler-get-object ((handler track-handler))
  (find-store-object (parse-url) :class 'track))

(defmethod handle-object ((handler track-handler) (track (eql nil)))
  (with-bknr-page (:title #?"bknr tracks")
    (:h2 "Artists: ")
    (:ul (dolist (artist (all-track-artists))
	   (html (:li (artist-html-link artist)))))
    (:h2 "Genres: ")
    (:ul (dolist (genre (all-track-genres))
	   (html (:li (genre-html-link genre)))))))

(defmethod handle-object ((handler track-handler) track)
  (let ((name (track-name track)))
    (with-bknr-page (:title #?"track: ${name}")
      (track :id (store-object-id track)))))

(defclass edit-track-handler (edit-object-handler track-handler)
  ())

(defmethod authorized-p ((handler edit-track-handler))
  (or (admin-p *user*)
      (user-has-flag *user* :music)))

(defmethod handle-object-form ((handler edit-track-handler) action
			       (track (eql nil)))
  (redirect "/edit-tracks"))

(defmethod handle-object-form ((handler edit-track-handler) action
			       track)
  (let ((name (track-name track)))
    (with-bknr-page (:title #?"edit track: ${name}")
      (track-form :id (store-object-id track)))))

(defmethod handle-object-form ((handler edit-track-handler) (action (eql :save))
			       track)
  (let ((genres (keywords-from-query-param-list (query-param-list "genre"))))
    (with-query-params (name artist album)
      (when artist (track-set-artist track artist))
      (when album (track-set-album track album))
      (when name (change-slot-values track 'name name))
      (when genres (track-set-genres track genres))
      (redirect (edit-object-url track)))))

(defmethod handle-object-form ((handler edit-track-handler) (action (eql :delete))
			       track)
  (when track
    (delete-object track))
  (redirect "/edit-track"))

(defclass tracks-handler (object-list-handler)
  ())

(defmethod handle-object ((handler tracks-handler) object)
  (let ((tracks (object-list-handler-get-objects handler object))
	(title (object-list-handler-title handler object)))
    (with-bknr-page (:title title)
      (:ul (dolist (track tracks)
	     (html (:li (html-link track))))))))

(defclass artist-tracks-handler (tracks-handler)
  ())

(defmethod object-list-handler-get-objects ((handler artist-tracks-handler) object)
  (let ((name (parse-url)))
    (get-artist-tracks name)))

(defmethod object-list-handler-title ((handler artist-tracks-handler) object)
  (format nil "tracks of ~a" (parse-url)))

(defclass genre-tracks-handler (keyword-handler tracks-handler)
  ())

(defmethod object-list-handler-get-objects ((handler genre-tracks-handler) genre)
  (get-genre-tracks genre))

(defmethod object-list-handler-title ((handler genre-tracks-handler) genre)
  (format nil "tracks of genre ~a" genre))

(defclass search-tracks-handler (edit-object-handler tracks-handler)
  ())

(defmethod authorized-p ((handler search-tracks-handler))
  t)

(defmethod object-list-handler-get-objects ((handler search-tracks-handler) object)
  (session-value :current-track-result))

(defmethod search-tracks ((handler search-tracks-handler))
  (with-query-params (artist album name genre operator)
    (let ((artists (when artist (find-matching-strings artist (all-track-artists))))
	  (genre (when genre (make-keyword-from-string genre)))
	  (albums (when album (find-matching-strings album (all-albums))))
	  (name-tracks (when name (let ((scanner (create-scanner name :case-insensitive-mode t)))
				    (remove-if-not #'(lambda (track) (scan scanner
									   (track-name track)))
					       (all-tracks)))))
	  (merge-op (case operator
		      (:or #'union)
		      (:and #'intersection)
		      (t #'union)))
	  tracks)
      (when artists
	(push (reduce #'union (mapcar #'get-artist-tracks artists)) tracks))
      (when albums
	(push (reduce #'union (mapcar #'album-tracks albums)) tracks))
      (when name
	(push name-tracks tracks))
      (when genre
	(push (get-genre-tracks genre) tracks))
      (setf (session-value :current-track-result) (reduce merge-op tracks)))))

(defmethod handle-object-form ((handler search-tracks-handler)
			       (action (eql :search)) object)
  (search-tracks handler)
  (call-next-method))

(defmethod handle-object-form ((handler search-tracks-handler)
			       action object)
  (let ((tracks (object-list-handler-get-objects handler object)))
    (with-bknr-page (:title "search tracks")
      (search-track)
      (:ul (dolist (track tracks)
	     (html (:li (html-link track))))))))

(defclass edit-tracks-handler (search-tracks-handler)
  ())

(defmethod object-handler-get-object ((handler edit-tracks-handler))
  (remove nil (mapcar #'(lambda (id) (find-store-object id :class 'track))
		      (query-param-list "track-id"))))

(defmethod handle-object-form ((handler edit-tracks-handler) action
			       (tracks (eql nil)))
  (with-bknr-page (:title "edit tracks")
    (search-track :title "search tracks to edit")
    (edit-track-collection)))

(defmethod handle-object-form ((handler edit-tracks-handler) (action (eql :search))
			       tracks)
  (search-tracks handler)
  (call-next-method))

(defmethod handle-object-form ((handler edit-tracks-handler) (action (eql :add-genres))
			       tracks)
  (let ((genres (keywords-from-query-param-list (query-param-list "genre"))))
    (dolist (track tracks)
      (store-object-add-keywords track 'genre genres))
    (redirect "/edit-tracks")))

(defmethod handle-object-form ((handler edit-tracks-handler) (action (eql :remove-genres))
			       tracks)
  (let ((genres (keywords-from-query-param-list (query-param-list "genre"))))
    (dolist (track tracks)
      (store-object-remove-keywords track 'genre genres))
    (redirect "/edit-tracks")))

(defmethod handle-object-form ((handler edit-tracks-handler) (action (eql :save-all))
			       tracks)
  (let ((genres (keywords-from-query-param-list (query-param-list "genre"))))
    (with-query-params (name artist album)
      (when artist (mapc #'(lambda (track) (track-set-artist track artist)) tracks))
      (when album (mapc #'(lambda (track) (track-set-album track album)) tracks))
      (when name (mapc #'(lambda (track) (change-slot-values track 'name name)) tracks))
      (when genres (mapc #'(lambda (track) (track-set-genres track genres)) tracks))
      (redirect "/edit-tracks"))))

(defmethod handle-object-form ((handler edit-tracks-handler) (action (eql :delete-all))
			       tracks)
  (mapc #'delete-object tracks)
  (redirect "/edit-tracks"))

(defclass playlist-handler (object-handler)
  ())

(defmethod object-url ((playlist playlist))
  (format nil "/playlist/~A" (playlist-name playlist)))

(defmethod html-link ((playlist playlist))
  (html ((:a :href (edit-object-url playlist)) (:princ-safe playlist))))

(defmethod object-handler-get-object ((handler playlist-handler))
  (find-store-object (parse-url) :class 'playlist))

(defmethod handle-object ((handler playlist-handler) (playlist (eql nil)))
  (with-bknr-page (:title #?"bknr playlists")
    (:ul (dolist (playlist (all-playlists))
	   (html (:li (html-link playlist)))))))

(defmethod handle-object ((handler playlist-handler) playlist)
  (let ((name (playlist-name playlist)))
    (with-bknr-page (:title #?"playlist: ${name}")
      (:ul (dolist (track (playlist-tracks playlist))
	     (html (:li (html-link track))))))))

