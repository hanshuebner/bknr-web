(in-package :bknr-user)

(enable-interpol-syntax)

(define-persistent-class comic ()
  ((name :read
	 :index-type string-unique-index
	 :index-reader comic-with-name
	 :index-values all-comics)
   (keywords :update :initform nil
                     :index-type hash-list-index
                     :index-reader get-keyword-comics)
   
   (mainurl :update)
   (imgregexp :update)
   (imgregexp-scanner :update :transient t)
   (mainregexp :update :initform nil)
   (mainregexp-scanner :update :transient t)
   (last-updated :update :transient t :initform 0)
   (url-hash :update :initform (make-hash-table :test #'equal))))

(defmethod initialize-transient-instance ((comic comic))
  (with-slots (mainurl mainurl-scanner
		       imgregexp imgregexp-scanner
		       mainregexp mainregexp-scanner) comic
    (when imgregexp
      (setf imgregexp-scanner (create-scanner imgregexp :case-insensitive-mode t)))
    (when mainregexp
      (setf mainregexp-scanner (create-scanner mainregexp :case-insensitive-mode t)))))

(defmethod print-object ((object comic) stream)
  (format stream "#<COMIC ~a ID: ~a>" (comic-name object)
	  (store-object-id object))
  object)

(deftransaction comic-get-image-uri (comic image-uri)
  (setf (gethash image-uri (comic-url-hash comic)) t))

(defun make-image-uri (image mainurl)
  (let* ((mainuri (parse-uri mainurl))
	 (uri (parse-uri image))
	 (host (uri-host mainuri))
	 (path (uri-parsed-path uri)))
    (cond ((uri-host uri) image)
	  ((eq :absolute (car path))
	   (concatenate 'string "http://" host image))
	  (t (render-uri (puri:merge-uris uri mainuri) nil)))))

#+(or)
(defmethod get-comic ((comic comic) &key force)
  (with-slots (name url mainurl imgregexp mainregexp) comic
    (let* ((date (daytag))
	   (image-name (format nil "~a-~a" name date))
	   (image (store-image-with-name image-name))
	   (parent-url mainurl))
      (if (and (not force)
	       (or image
		   (<= (get-universal-time) (+ (comic-last-updated comic) (* 60 24)))))
	  image
	  (progn
	    (setf (comic-last-updated comic) (get-universal-time))
	    ;; get main page
	    (multiple-value-bind (page code)
		(net.aserve.client:do-http-request mainurl)
	      (unless (= code 200)
		(error "Could not get ~a" mainurl))
	      
	      (when mainregexp
		(multiple-value-bind (match substrings)
		    (scan-to-strings (comic-mainregexp-scanner comic) page)
		  (unless match
		    (error "Could not get second page from ~a" mainurl))
		  (let ((second-uri (make-image-uri (aref substrings 0) mainurl)))
		    (multiple-value-bind (page2 code)
			(net.aserve.client:do-http-request second-uri
			  :headers (list (cons "Referrer" mainurl)))
		      (unless (= code 200)
			(error "Could not get second page ~a" second-uri))
		      (setf page page2
			    parent-url second-uri)))))
	      
	      (multiple-value-bind (match substrings)
		  (scan-to-strings (comic-imgregexp-scanner comic) page)
		(unless match
		  (error "Could not get comic image from ~a" mainurl))
		
		(let ((image-uri (make-image-uri (aref substrings 0) parent-url)))
		  (format t "image-uri: ~a, hash value ~A ~%"
			  image-uri (gethash image-uri (comic-url-hash comic)))
		  (unless (gethash image-uri (comic-url-hash comic))
		    ;; get image
		    (multiple-value-bind (page code headers)
			(net.aserve.client:do-http-request image-uri :format :binary)
		      (unless (= code 200)
			(error "Could not get ~a" image-uri))
		      (comic-get-image-uri comic image-uri)
		      (let ((content-type (cdr (assoc "content-type" headers :test #'string-equal))))
			(unless (and content-type
				     (member content-type
					     '("image/jpeg" "image/gif" "image/png")
					     :test #'string-equal))
			  (error "Image ~a of wrong type" image-uri))
			(let ((image (make-instance 'store-image
                                                    :name image-name
                                                    :timestamp (get-universal-time)
                                                    :keywords (list (make-keyword-from-string name)
                                                                    :comic)
                                                    :type (image-type-symbol content-type))))
			  (when image
			    (blob-from-array image page)
			    (with-store-image (i image)
			      (change-slot-values image
						  'width (cl-gd:image-width i)
						  'height (cl-gd:image-height i)))
			    image)))))))))))))
  
(defun get-all-comics (&key (debug nil) force)
  (mapcar #'(lambda (comic)
	      (if debug
		  (get-comic comic :force force)
		  (ignore-errors (get-comic comic :force force))))
	  (all-comics)))


#+(or)
(unless (cron-job-with-name "update comics")
  (make-instance 'cron-job :name "update comics"
                 :minute 0
                 :hour 11
                 :job 'get-all-comics))

#|

(make-instance 'comic
               :name  "userfriendly"
               :mainurl  "http://ars.userfriendly.org/"
               :imgregexp  "src=\"(http://www.userfriendly.org/cartoons/archives/[0-9]*[a-z]*/xuf[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "wulffmorgenthaler"
               :mainurl  "http://www.wulffmorgenthaler.com/thestrip.asp"
               :imgregexp  "src=\"(log/DD[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "jerkcity" 
               :mainurl  "http://www.jerkcity.com/" 
               :imgregexp  "src=\"(\/jerkcity[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "sinfest" 
               :mainurl  "http://www.sinfest.net/" 
               :imgregexp  "src=\"(\/comics\/sf[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "nonsequitur" 
               :mainurl  "http://www.non-sequitur.com/" 
               :imgregexp  "src=\"(http:\/\/images.ucomics.com/comics/nq/[0-9]*/nq[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "megatokyo" 
               :mainurl  "http://www.megatokyo.com/" 
               :imgregexp  "src=\"(/strips/[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "pennyarcade" 
               :mainurl  "http://www.penny-arcade.com/view.php3" 
               :imgregexp  "src=\"(images/[0-9]*/.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "collegeroomies" 
               :mainurl  "http://crfh.net/" 
               :imgregexp  "src=\"(/comics/crfh[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "rpgworld" 
               :mainurl  "http://www.rpgworldcomic.com/" 
               :imgregexp  "src=\"(/comics/[0-9]*.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "brunothebandit" 
               :mainurl  "http://www.brunothebandit.com/"
               :imgregexp  "src=\"(/comics/[0-9]*.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "itswalky" 
               :mainurl  "http://www.itswalky.com/"
               :imgregexp  "src=\"(/comics/[0-9]*.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "dieselsweeties" 
               :mainurl  "http://www.dieselsweeties.com/"
               :imgregexp  "src=\"(http://images.clango.org/strips/.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "choppingblock" 
               :mainurl  "http://www.choppingblock.org/"
               :imgregexp  "src=\"(/comics/cb[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "sheldon" 
               :mainurl  "http://www.comics.com/comics/sheldon/index.html"
               :imgregexp  "src=\"(/comics/sheldon/archive/images/sheldon[0-9]*.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "shawisland" 
               :mainurl  "http://students.washington.edu/durandal/shawisland/"
               :imgregexp  "src=\"(strips/shaw[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "residencelife" 
               :mainurl  "http://www.reslifecomics.com"
               :imgregexp  "src=\"(http://www.reslifecomics.com/comics/[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "lostfound" 
               :mainurl  "http://www.lostandfoundcomic.com/"
               :imgregexp  "src=\"(/comics/[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "reallife" 
               :mainurl  "http://www.reallifecomics.com/"
               :imgregexp  "src=\'(/index.php.*)\'")
(make-instance 'comic
               :name  "striptease" 
               :mainurl  "http://www.stripteasecomic.com/"
               :imgregexp  "src=\"(strips2002/[0-9]*\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "littlegamers" 
               :mainurl  "http://www.little-gamers.com/"
               :imgregexp  "src='(/index.php\?.*)'")
(make-instance 'comic
               :name  "jackiesfridge" 
               :mainurl  "http://jackiesfridge.keenspace.com"
               :imgregexp  "src=\"(http://.*/comics/jf[0-9]*.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "langlang" 
               :mainurl  "http://langlang.keenspace.com/right.html"
               :imgregexp  "src=\"(/comics/[0-9]*.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "tang" 
               :mainurl  "http://tang.keenspace.com"
               :imgregexp  "src=\"(http://.*/comics/[0-9]*.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "purplepussy" 
               :mainurl  "http://purplepussy.net/"
               :imgregexp  "src=\"(/comics/puss[0-9]*.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "nichtlustig" 
               :mainurl  "http://www.nichtlustig.de/main.html"
               :mainregexp  "href=\"(toondb/[0-9]*.html)\" alt=\"zu den Cartoons\""
               :imgregexp  "src=\"(../comics/full/[0-9]*.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "underpower"
               :mainurl  "http://underpower.non-essential.com"
               :imgregexp  "src=\"(comics/[0-9]*.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "gpf"
               :mainurl  "http://www.gpf-comics.com/"
               :imgregexp  "src=\"(/comics/gpf[0-9]*.*?\.(gif|png|jpg))\"")
(make-instance 'comic
               :name  "applegeeks"
               :mainurl  "http://www.applegeeks.com/"
               :imgregexp  "src=\"(/comics/issue[0-9]*.*?\.(gif|png|jpg))\"" )
(make-instance 'comic
               :name  "bobandgeorge"
               :mainurl  "http://www.bobandgeorge.com"
               :imgregexp  "src=\"([0-9]+.(gif|png|jpg))\"" )
(make-instance 'comic
               :name  "machall"
               :imgregexp  "src='(/index.php.do_command=show_strip&strip_id=[0-9].*)'"
               :mainurl  "http://www.machall.com/" )
(make-instance 'comic
               :name  "kungfool"
               :imgregexp  "src=\"(images/CK_comics/[0-9_]+.(gif|png|jpg))\""
               :mainurl  "http://kungfool.transpect.net/index.php" )
(make-instance 'comic
               :name  "ivy"
               :mainurl  "http://ivy.de/"
               :imgregexp  "src=\"([0-9]+/img/ib/[0-9]+.(png|jpg|gif))\"" )
(make-instance 'comic
               :name  "spooner"
               :mainurl  "http://www.hookedoncomics.com/spooner.php"
               :imgregexp  "src=\"(strips/[0-9]+.(gif|png|jpg))\"" )
(make-instance 'comic
               :name  "vgcat"
               :mainurl  "http://www.vgcats.com/vgc_comics/" 
               :imgregexp  "src=\"(images/[0-9]+.(gif|png|jpg))\"" )
(make-instance 'comic
               :name  "samandfuzzy"
               :mainurl  "http://www.samandfuzzy.com/"
               :imgregexp  "src=\"(comics/[0-9]+.(jpg|png|gif))\"" )
(make-instance 'comic
               :name  "butternutsquash"
               :mainurl  "http://www.butternutsquash.net/assets/pages/bns-current.html"
               :imgregexp  "src=\"(../library/[0-9]+-bns.(jpg|png|gif))\"" )
(make-instance 'comic
               :name  "somethingpositive"
               :mainurl  "http://www.somethingpositive.net/"
               :imgregexp  "src=\"(arch/sp[0-9]+.(gif|png|jpg))\"" )
(make-instance 'comic
               :name  "boasas"
               :mainurl  "http://www.boasas.com/" 
               :imgregexp  "src=\"(boasas/[0-9]+.(png|jpg|gif))\"" )
(make-instance 'comic
               :name  "lizard"
               :mainurl  "http://www.lizardcomics.com/" 
               :imgregexp  "SRC=\"(/comics/liz[0-9]+.(jpg|png|gif))\"" )
 

|#
