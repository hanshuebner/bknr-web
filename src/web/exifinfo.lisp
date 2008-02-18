;; Exif info parser, written by Kevin Layer.
;; This code is in the public domain.  You may do with it what you
;; want.
;;
;; Required software:
;; - Allegro CL: probably any version will do, but I used ACL 6.0.
;;   http://www.franz.com
;;
;; What is Exif?
;; From http://www.pima.net/standards/it10/PIMA15740/exif.htm:
;;
;;  The Exif (Exchangeable Image File) format is a JEIDA (Japan Electronic
;;  Industry Development Association, http://www.jeida.or.jp/index-e.html)
;;  standard that was established in Oct. 1995, revised in Nov. 1997 as
;;  version 2.0, and revised in June 1998 as version 2.1. Exif is referenced
;;  as a preferred image format for digital cameras in ISO 12234-1
;;  Photography - Electronic still picture cameras -- Removable memory -
;;  Part 1: Basic removable memory reference model
;;  (http://www.pima.net/standards/it10/IT10_POW.htm#12234-1).  PIMA thanks
;;  JEIDA for allowing us to distribute the JEIDA Exif specification from
;;  our website.
;;
;;    http://www.pima.net/standards/it10/PIMA15740/Exif_2-1.PDF
;;
;;  Most current digital camera store images using Exif compressed files.
;;  Exif compressed files use the baseline JPEG DCT format specified in
;;  ISO/IEC 10918-1 (http://www.iso.ch/cate/d18902.html). This means the
;;  image data can be read by any application supporting "JPEG", including
;;  essentially all web browsers and image editing, desktop presentation,
;;  and document creation applications. In addition, Exif/JPEG stores
;;  metadata within application segments at the beginning of the file, and
;;  uses sRGB (http://www.w3.org/Graphics/Color/sRGB.html) as the default
;;  color space.
;;
;; My understanding of Exif actually came from reading the source code
;; of a program written by Matthias Wandel called `jhead' obtained from
;; http://www.sentex.net/~mwandel/jhead/.  According to that page the code
;; is in the public domain. Thanks Matthias.
;;
;; The following code is known to work on files produced by the following
;; digital cameras:
;; - Nikon Coolpix 950 and 990
;; - Kodak 210 Zoom
;; - Canon D30
;;
;; How does this stuff work?
;; 
;;  cl-user(2): (use-package :util.exif)
;;  t
;;  cl-user(3): (setq x (parse-exif-data "~/test.jpg"))
;;  #S(exif-info :file "~/test.jpg" :make "Canon" :model "Canon EOS D30"
;;               :date "2001:01:23 22:57:08" :comment "" :exposure 0.016666668
;;               :f-number 5.6 :iso-rating 100 :exposure-bias-value 0.0
;;               :subject-distance 2.1 :flash t :focal-length 132.0
;;               :image-width 100 :image-length 66)
;;  cl-user(4): (exif-info-model x)
;;  "Canon EOS D30"
;;  cl-user(5): (exif-info-date x)
;;  "2001:01:23 22:57:08"
;;  cl-user(6): (exif-info-flash x)
;;  t
;;  cl-user(7): (exif-info-focal-length x)
;;  132.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; $Id$

(defpackage :util.exif
  (:use :common-lisp :excl)
  (:export
   #:exif-info				; type
   
   #:make-exif-info			; accessor
   #:exif-info-file			; accessor
   #:exif-info-make			; accessor
   #:exif-info-model			; accessor
   #:exif-info-date			; accessor
   #:exif-info-comment			; accessor
   #:exif-info-exposure			; accessor
   #:exif-info-f-number			; accessor
   #:exif-info-iso-rating		; accessor
   #:exif-info-exposure-bias-value	; accessor
   #:exif-info-subject-distance		; accessor
   #:exif-info-flash			; accessor
   #:exif-info-focal-length		; accessor
   #:exif-info-image-width		; accessor
   #:exif-info-image-length		; accessor

   #:parse-exif-data			; function
   ))

(in-package :util.exif)

(defparameter *debug-exifdump* nil)

(defstruct exif-info
  file
  ;; Exif tags:
  make					; Make
  model					; Model
  date					; DateTime
  comment				; Comments (?)
  exposure				; ExposureTime
  f-number				; FNumber
  iso-rating				; ISOSpeedRatings
  exposure-bias-value			; ExposureBiasValue
  subject-distance			; SubjectDistance
  flash					; Flash
  focal-length				; FocalLength
  image-width				; ExifImageWidth
  image-length				; ExifImageLength
  )

(defvar *endian* nil)

(defun get16u (data offset)
  (if* (eq :motorola *endian*)
     then (logior (ash (aref data offset) 8)
		  (aref data (1+ offset)))
     else (logior (ash (aref data (1+ offset)) 8)
		  (aref data offset))))

(defun get32u (data offset)
  (let* ((b0 (aref data offset))
	 (b1 (aref data (incf offset)))
	 (b2 (aref data (incf offset)))
	 (b3 (aref data (incf offset)))
	 (val (if* (eq :motorola *endian*)
		 then (logior (ash b0 24) (ash b1 16) (ash b2 8) b3)
		 else (logior (ash b3 24) (ash b2 16) (ash b1 8) b0))))
    val))

(defun get32s (data offset)
  (let ((val (get32u data offset)))
    (when (logbitp 31 val) (setq val (- val #x100000000)))
    val))

(defun get16m (data offset)
  (logior (ash (aref data offset) 8)
	  (aref data (1+ offset))))

(defvar *debug* nil)

(defun parse-exif-data (file)
  (let (marker data length res marker-position)
    (with-open-file (s file :direction :input
		     :element-type '(unsigned-byte 8))
      (when (not (and (eql #xff (read-byte s nil nil))
		      (eql #xd8 ;; SOI (start of image)
			   (read-byte s nil nil))))
	(error "~a: not a Jpeg file." file))
      
      ;; read until we find the end or the Exif section
      (loop
	;; read up to 8 pad chars
	(dotimes (i 8)
	  (setq marker-position (file-position s))
	  (setq marker (read-byte s nil nil))
	  (when (null marker)
	    (error "~a: EOF reading section from file." file))
	  (when (/= #xff marker) (return)))
	
	(when (= #xff marker) (error "~a: too many padding bytes!" file))
	
	(let ((lh (read-byte s))
	      (ll (read-byte s)))
	  (setq length (logior (ash lh 8) ll))
	  (when (< length 2) (error "~a: invalid marker." file))
	  (setq data (make-array length :element-type '(unsigned-byte 8)))
	  (setf (aref data 0) lh)
	  (setf (aref data 1) ll))
	
	(when (/= length (read-sequence data s :start 2))
	  (error "~a: could not read ~d bytes of header." file length))
	
	(case marker
	  (#xe0 ;; JFIF, ignore it
	   (when *debug*
	     (format t "found e0 @ 0~o, length ~d~%" marker-position length)))
	  (#xe1 ;; APP1 marker w/EXIF info, process it
	   (when *debug*
	     (format t "found e1 @ 0~o, ~d bytes~%" marker-position length))
	   (setq res (process-exif file data length res))
;;;; For photoshop 7 written images, the size following the first APP1
;;;; marker doesn't seem to work.  Skipping over that many bytes in the
;;;; image doesn't lead to another marker, like it should.  I'm probably
;;;; missing something...  in any case, it doesn't appear that the SOF
;;;; markers need to be processed to get image dimensions, either on images
;;;; out of my D30 or written by PS7.  -12/26/02
	   (return res))
	  (#xda
	   ;; start of image, there was no EXIF section...
	   (when *debug* (format t "found da @ 0~o~%" marker-position))
	   (return nil))
	  ((#xc0 #xc1 #xc2 #xc3 #xc5 #xc6 #xc7 #xc9 #xca #xcb #xcd #xce #xcf)
	   ;; start of frame (0-15).  Useful to get image dimensions.
	   (when *debug* (format t "found SOF @ 0~o~%" marker-position))
	   (setq res (process-sof file data res)))
	  (t
	   (when *debug-exifdump*
	     (format t "Unknown marker: 0x~x.~%" marker)))))
      res)))

(defun process-sof (file data res
		    &aux (init-forms nil))
  (let ((len (get16m data 3))
	(wid (get16m data 5)))
    (if* res
       then (setf (exif-info-image-length res) len)
	    (setf (exif-info-image-width res) wid)
	    res
       else (push len init-forms)
	    (push :image-length init-forms)
	    (push wid init-forms)
	    (push :image-width init-forms)
	    (apply #'make-exif-info :file file init-forms))))

(defun process-exif (file data length res
		     &aux (*endian* nil))
  (when (not (equalp (subseq data 2 8)
		     #(#.(char-code #\E)
		       #.(char-code #\x)
		       #.(char-code #\i)
		       #.(char-code #\f)
		       0
		       0)))
    (error "~a: incorrect Exif header (got: ~a)." file
	   (let ((*print-base* 16.))
	     (format nil "~a" (subseq data 2 8)))))

  (setq *endian*
    (if* (equalp (subseq data 8 10)
		 #(#.(char-code #\I) #.(char-code #\I)))
       then :intel
     elseif (equalp (subseq data 8 10)
		    #(#.(char-code #\M) #.(char-code #\M)))
       then :motorola
       else (error "~a: invalid Exif alignment marker." file)))
  
  (when (or (not (= #x2a (get16u data 10)))
	    (not (= #x08 (get32u data 12))))
    (error "~a: invalid Exif start." file))
  
  (process-exif-dir (setq res (or res (make-exif-info :file file)))
		    file data 16 8 (- length 6))
  res)

(defparameter *bytes-per-format* #(0 1 1 2 4 8 1 1 2 4 8 4 8))

(defun process-exif-dir (res file data dir-start-offset
			 offset-base-offset length)
  (let ((n-dir-entries (get16u data dir-start-offset)))
    (when (> (+ dir-start-offset 2 (* 12 n-dir-entries))
	     (+ offset-base-offset length))
      (error "~a: illegally sized directory." file))
    
    (dotimes (de n-dir-entries)
      (let* ((dir-entry (+ dir-start-offset 2 (* 12 de)))
	     (tag (get16u data dir-entry))
	     (format (get16u data (+ dir-entry 2)))
	     (components (get32u data (+ dir-entry 4)))
	     byte-count
	     value-offset)
	(when (> (1- format) 12)
	  (error "~a: illegal format code in EXIF directory: ~d." file format))
	
	(setq byte-count
	  (* components (aref *bytes-per-format* format)))
	
	(if* (> byte-count 4)
	   then (let ((offset-val (get32u data (+ dir-entry 8))))
		  ;; If its bigger than 4 bytes, the dir entry contains an
		  ;; offset.
		  (when (> (+ offset-val byte-count) length)
		    ;; Bogus pointer offset and / or bytecount value
		    (error
		     "~a: illegal ptr offset (offset ~d bytes ~d length ~d)."
		     file offset-val byte-count length))
		  (setq value-offset
		    (+ offset-base-offset offset-val)))
	   else ;; 4 bytes or less and value is in the dir entry itself
		(setq value-offset (+ dir-entry 8)))
	
	(case tag
	  (#x10F ;; :make
	   (setf (exif-info-make res)
	     (get-string data value-offset (+ value-offset 31))))
	  (#x110 ;; :model
	   (setf (exif-info-model res)
	     (get-string data value-offset (+ value-offset 39))))
	  (#x9003 ;; :date
	   (setf (exif-info-date res)
	     (get-string data value-offset (+ value-offset 19))))
	  (#x9286 ;; :comment
	   (setf (exif-info-comment res)
	     (get-string data value-offset (+ value-offset 199))))
	  (#x829A ;; :exposure
	   (setf (exif-info-exposure res)
	     (convert-any-format data value-offset format)))
	  (#x829D ;; :f-number
	   (setf (exif-info-f-number res)
	     (convert-any-format data value-offset format)))
	  (#x8827 ;; :iso-rating
	   (setf (exif-info-iso-rating res)
	     (convert-any-format data value-offset format)))
	  ((#x9202 #x9205) ;; :aperture-value and :max-aperture-value
	   (when (not (exif-info-f-number res))
	     (error "foo: ~s" (convert-any-format data value-offset format))))
	  (#x9204 ;; :exposure-bias-value
	   (setf (exif-info-exposure-bias-value res)
	     (convert-any-format data value-offset format)))
	  (#x9206 ;; :subject-distance
	   (setf (exif-info-subject-distance res)
	     (convert-any-format data value-offset format)))
	  (#x9209 ;; :flash
	   (setf (exif-info-flash res)
	     (/= 0 (convert-any-format data value-offset format))))
	  (#x920A ;; :focal-length
	   (setf (exif-info-focal-length res)
	     (convert-any-format data value-offset format)))
	  (#xA002 ;; :image-width
	   (setf (exif-info-image-width res)
	     (convert-any-format data value-offset format)))
	  (#xA003 ;; :image-length
	   (setf (exif-info-image-length res)
	     (convert-any-format data value-offset format)))
;;;; I don't believe the following are needed.
;;;	  (#x11A ;; x-resolution
;;;	   (format t "x res: ~s~%"
;;;		   (convert-any-format data value-offset format)))
;;;	  (#x11B ;; y-resolution
;;;	   (format t "y res: ~s~%"
;;;		   (convert-any-format data value-offset format)))
	  (t
	   (when *debug-exifdump*
	     (format t "Unknown tag #x~x~%" tag))))
	
	(when (or (= tag #x8769) ;; Exif IFD pointer
		  (= tag #xa005) ;; Interoperability IFD pointer
		  )
	  (let ((subdir-start-offset
		 (+ offset-base-offset (get32u data value-offset))))
	    (when (or (< subdir-start-offset offset-base-offset)
		      (> subdir-start-offset
			 (+ offset-base-offset length)))
	      (error "~a: illegal directory link." file))
	    (process-exif-dir
	     res file data subdir-start-offset offset-base-offset
	     length)))))))

#-allegro
(defun octets-to-string (buff &key (start 0) (end (length buff)))
  ;; extract a subsequence of the usb8 buff and return it as a string
  (let ((str (make-string (- end start))))
    (do ((i start (1+ i))
	 (ii 0 (1+ ii)))
	((>= i end))
      (setf (schar str ii) 
	(code-char (aref buff i))))
    str))

(defun get-string (data start end)
  (do ((s start (1+ s)))
      ((= s end) (octets-to-string data :start start :end end))
    (when (= 0 (aref data s))
      (return (octets-to-string data :start start :end s)))))

(defun convert-any-format (data offset format)
  (ecase format
;;;; Never used, in my experience:
;;;    (6 ;; sbyte -- gone in EXIF 2.2?
;;;     )
;;;    (1 ;; byte
;;;     )
;;;    (2 ;; string
;;;     )
    (3 ;; short
     (get16u data offset))
    (4 ;; long
     (get32u data offset))
    ((5 10) ;; rational and srational
     (let ((num (get32s data offset))
	   (den (get32s data (+ offset 4))))
       (if* (= 0 den)
	  then 0
	  else (float (/ num den)))))
;;;; Never used, in my experience:
;;;    (7 ;; undefined
;;;     )
;;;    (8 ;; sshort -- gone in EXIF 2.2?
;;;     )
;;;    (9 ;; slong
;;;     )
;;;    (11 ;; single -- gone in EXIF 2.2?
;;;     )
;;;    (12 ;; double -- gone in EXIF 2.2?
;;;     )
    ))

(provide :exifinfo)
