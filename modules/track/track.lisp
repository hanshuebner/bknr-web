(in-package :bknr-user)

(enable-interpol-syntax)

#+nil
(define-persistent-class album ()
  ((name :string :read)
   (tracks '(:track) :update :initform nil)))

#+nil
(deftransaction album-add-track (album track)
  (pushnew track (album-tracks album)))

#+nil
(deftransaction album-remove-track (album track)
  (setf (album-tracks album)
	(remove track (album-tracks album))))

#+nil
(defmethod album-artists ((album album))
  (remove-duplicates (mapcar #'track-artist (album-tracks album))))

#+nil
(defmethod album-genres ((album album))
  (remove-duplicates (mapcan #'track-genre (album-tracks album))))

(define-persistent-class track (owned-object blob)
  ((name :read :initform nil)
   (artist :update :initform nil
                   :index-type hash-index :index-initargs (:test #'equal)
                   :index-reader get-artist-tracks
                   :index-keys all-track-artists)
   
   (album :update :initform nil)

   (genre :update :initform nil
                  :index-type hash-list-index
                  :index-reader get-genre-tracks
                  :index-keys all-track-genres)))

(defmethod print-object ((track track) stream)
  (format stream "#<~A ARTIST: ~S NAME: ~S ID: ~A>"
	  (class-name (class-of track))
	  (track-artist track)
	  (track-name track)
	  (store-object-id track))
  track)

(defun trim (string)
  (when string
    (regex-replace #?r"^\s*(.*?)\s*$" string #?r"\1")))

(defun import-mp3 (pathname &key (name (pathname-name pathname)) artist album genre)
  (with-id3-tags (tit2 tpe1 talb tcon) pathname
                 (when tit2 (setf name tit2))
                 (when (and tpe1 (not artist))
                   (setf artist tpe1))
                 (when (and talb (not album))
                   (setf album talb))
                 (when (and tcon (not genre))
                   (let ((genre-id (parse-integer tcon :junk-allowed t)))
                     (push (make-keyword-from-string
                            (if genre-id
                                (get-id3-genre genre-id)
                                tcon))
                           genre)))
                 (let ((mp3 (apply #'make-instance
                                   (list 'track
                                         :type :MP3
                                         :timestamp (get-universal-time)
                                         :name (trim name)
                                         :artist (trim artist)
                                         :album (trim album)
                                         :genre genre))))
                                        ;      (blob-from-file mp3 pathname)
                   (rename-file-to-blob mp3 pathname)
                   mp3)))

(defun import-from-ripper (genre &optional (fifo-name "/var/run/bknr-import-mp3"))
  (with-open-file (fifo fifo-name)
    (loop
       (let ((track (import-mp3 (read-line fifo) :genre genre)))
         (format t "; imported ~a~%" track)))))

(defun import-mp3-directory (&key (spool *user-spool-directory-root*)
			     genre album artist
			     tags-from-dir)
  (let ((path-spool (cdr (pathname-directory spool))))
    (loop for file in (directory (merge-pathnames "**/*.mp3" spool))
       do (when tags-from-dir
            (let ((dir (subseq (cdr (pathname-directory file))
                               (1+ (length path-spool)))))
              (mapc #'(lambda (name type)
                        (case type
                          (:artist (setf artist name))
                          (:album (setf album name))
                          (:genre (setf genre (list (make-keyword-from-string name))))))
                    dir tags-from-dir)))
       collect (handler-case
                   (let ((mp3 (import-mp3 file :artist artist :album album :genre genre))
                         (file-directory (make-pathname :defaults file :name nil :type nil)))
                     (when mp3
                                        ;			  (delete-file file)
                       (when (directory-empty-p file-directory)
                         #-allegro
                         (#+cmu unix:unix-rmdir #+sbcl sb-posix:rmdir (namestring file-directory))
                         #+allegro
                         (delete-directory file-directory)))
                     (cons file mp3))
                 (error (e)
                   (cons file e))))))

(defun all-tracks ()
  (store-objects-with-class 'track))

(defun all-albums ()
  nil)

(defun album-tracks (album)
  nil)

(defun get-genres-intersection-tracks (genres)
  (reduce #'intersection (mapcar #'get-genre-tracks genres)))

(defun get-genres-union-tracks (genres)
  (reduce #'union (mapcar #'get-genre-tracks genres)))

(deftransaction track-set-artist (track artist)
  (setf (track-artist track) artist))

(deftransaction track-set-album (track album)
  (setf (track-album track) album))

(deftransaction track-set-genres (track genres)
  (setf (track-genre track) genres))

(define-persistent-class playlist (owned-object)
  ((tracks :update :initform nil)
   (name :read)))

(deftransaction playlist-add-tracks (playlist new-tracks)
  (with-slots (tracks) playlist
    (setf tracks (append tracks new-tracks))))

(deftransaction playlist-pop-track (playlist)
  (pop (playlist-tracks playlist)))

(defun all-playlists ()
  (store-objects-with-class 'playlist))

(define-persistent-class track-votes ()
  ((track :read
	  :index-type hash-index
	  :index-reader track-votes)
   (user :read
	 :index-type hash-index
	 :index-reader user-votes)))
