(in-package :bknr.images)

(enable-interpol-syntax)

(define-bknr-tag image-page (results &key)
  (let* ((num-pages (length results))
	 (page (parse-integer (or (query-param "page") "0"))))
    (if (>= page num-pages)
	(html "No such page")
	(let ((images (nth page results)))
	  (image-collection images)
	  (dotimes (i num-pages)
	    (html (:princ "&nbsp;")
		  (if (= i page)
		      (html (:princ-safe i))
		      (html (cmslink (format nil "~A?page=~A" (script-name) i) (:princ-safe i))))
		  (:princ "&nbsp;")))))))

(define-bknr-tag banner (&key link keyword width height)
  (let* ((images (get-keywords-intersection-store-images (list :banner
							       (if (keywordp keyword)
								   keyword
								   (make-keyword-from-string keyword)))))
	 (banner (random-elt images)))
    (when banner
      (unless width
	(setf width (store-image-width banner)))
      (unless height
	(setf height (store-image-height banner)))
      (loop while (and (< width 400)
		       (< height 400))
	    do (setq height (* 2 height)
		     width (* 2 width)))
      (html ((:div :class "banner")
	     (if link
		 (html ((:a :href link)
			((:img :src (format nil "/image/~A"
					    (store-object-id banner))
			       :width width :height height))))
		 (html ((:img :src (format nil "/image/~A"
					   (store-object-id banner))
			      :width width :height height)))))))))

(define-bknr-tag random-image (&key keywords (transform ""))
  (let* ((keywords (mapcar #'make-keyword-from-string (split "," keywords)))
	 (images (get-keywords-intersection-store-images keywords))
	 (image (random-elt images)))
    (when image
      (html ((:img :src (format nil "/image/~a~a" (store-object-id image) transform)))))))

(define-bknr-tag user-image (&key user)
  (let* ((user (find-user user))
	 (image (when user (random-elt (user-images user)))))
    (when image
      (html ((:div :class "user-picture"))
	    ((:img :src (format nil "/image/~A/thumbnail,,120,120"
				(store-object-id image))))))))

;;; von der GPN
#+nil
(define-bknr-tag user-image (&key user)
  (let ((user (find-user user)))
    (when user
      (let* ((image (random-elt (user-images user))))
	(when image
	  (html ((:div :class "user-picture")
		 ((:img :src (format nil "/image/~A/thumbnail,,120,120"
				     (store-object-id image)))))))))))
    
(defun image-collection (images &key title (url "/browse-image/"))
  (html ((:div :class "images")
	 (when title
	   (html (:h2 (:princ-safe title))))
	 (:table (loop for image-row on images by #'(lambda (seq) (subseq seq 5))
		       do (html (:tr (loop for image in (subseq image-row 0 5)
					   for image-id = (store-object-id image)
					   for image-name = (store-image-name image)
					   do (html ((:td :width "110")
						     ((:a :href #?"${url}${image-id}")
						      ((:img :src (image-url image :process "thumbnail")
							     :alt image-name)))))))))))))

(defun layouter (images)
  (image-collection
   (let ((keyword-hash (make-hash-table))
	 keywords
	 seen-images)
     (loop for image in images
	   do (loop for keyword in (store-image-keywords image)
		    unless (find image seen-images)
		    do (if (nth-value 1 (gethash keyword keyword-hash))
			   (push image (gethash keyword keyword-hash))
			   (setf (gethash keyword keyword-hash) (list image)))
		    (setf keywords (remove-duplicates (cons keyword keywords)))
		    (push image seen-images)))
     (let ((keywords (sort (copy-list keywords) #'>
			   :key #'(lambda (keyword) (length (gethash keyword
								     keyword-hash))))))
       (loop for keyword in keywords
	     for images = (sort (gethash keyword keyword-hash) #'< :key #'store-image-aspect-ratio)
	     nconc images)))))

(define-bknr-tag reset-results ()
  (setf (session-value :current-query-name) nil
	(session-value :current-thumbnail-layout) nil
	(session-value :current-query-result) nil))

(defun edit-image-collection (&key title (images (session-value :current-query-result)))
  (format t "class ~a subtypep ~a~%" (class-of (first images)) (subtypep (class-of (first images)) (find-class 'store-image)))
  (unless (subtypep (class-of (first images)) (find-class 'store-image))
    (reset-results)
    (setf images nil))
  (html	((:div :class "edit-images")
	 ((:form :method "POST")
	  (when title
	    (html (:h2 (:princ-safe title))))
	  ((:table :class "keywords")
	   (:tr (:td (:h3 "Edit keywords of selected images:")))
	   (:tr (:td (image-keyword-choose-dialog :create t)))
	   (:tr (:td (submit-button "add-keywords" "Add keywords!"))
	        (:td (submit-button "remove-keywords" "Remove keywords!")))
	   (:br)
	   ((:input :type "button"
		    :name "check-all"
		    :value "Select all images"
		    :onClick "check(this,'image-id',true);"))
	   ((:input :type "button"
		    :name "uncheck-all"
		    :value "Unselect all images"
		    :onClick "check(this,'image-id',false);"))
	   ((:table :class "images")
	    (loop for image-row on images by #'(lambda (seq) (subseq seq 5))
		  do (html (:tr (loop for image in (subseq image-row 0 5)
				      for image-id = (store-object-id image)
				      for image-name = (store-image-name image)
				      do (html ((:td)
						((:div :align "center")
						 (:princ-safe image-name)(:br)
						 ((:a :href
						      (format nil #?"~a/${image-id}"
							      "/edit-image"))
						  ((:img :src (image-url image :process "thumbnail")
							 :alt image-name)))
						 ((:input :type "checkbox" :name "image-id" :value #?"${image-id}" :checked "checked"))
						 (:br)
						 (image-keyword-choose-dialog :create t :size "1" :name "keyword-img")))))))))
	   (submit-button "assign-individual-keywords" "Change keywords on selected images!")
	   (submit-button "delete" "Delete selected images"
			  :confirm "Really delete images?"))))))

(define-bknr-tag search-image-collection (&key title)
  "Generates a search field to search the image collection

Arguments: title (required) the title of the search field

Outputs: A search field to do name search and keyword search on the image collection
"
  (html ((:div :class "search-images")
	 ((:form :method "POST")
	  (when title
	    (html (:h2 (:princ-safe title))))
	  ((:table :class "search-panel")
	   (:tr (:td "image name: " ((:input :type "text" :length "15" :name "name")))
		(:td (:h3 "keyword"))
		(:td (select-box "operator" '(("and" "and") ("or" "or")) :default "and")
		     (image-keyword-choose-dialog :create t))
		(:td (submit-button "search" "Search images!"))))
	  (:br)))))

(define-bknr-tag image-browser (&key id)
  "Generates an image browser for an image

Arguments: id (required) the id of the image to browse

Outputs: An image browser
"
  (let ((image (store-object-with-id (if (numberp id) id (parse-integer id)))))
    (html
     (:p ((:table :border "1")
	  (:tr (:td "full-image") (:td ((:a :href (format nil "/image/~a" id))
					(:princ-safe (store-image-name image)))))
	  (:tr (:td "timestamp") (:td (:princ-safe (format-date-time (blob-timestamp image)))))
	  (:tr (:td "width") (:td (:princ-safe (store-image-width image))))
	  (:tr (:td "height") (:td (:princ-safe (store-image-height image))))
	  (:tr (:td "aspect ratio") (:td (:princ-safe (store-image-aspect-ratio image))))
	  (:tr (:td "keywords") (:td (loop for keyword in (mapcar #'string-downcase (store-image-keywords image))
					   do (cmslink (concatenate 'string "/image-keyword/" keyword)
						(:princ keyword)))))))
     (:p ((:img :src (image-url image :process "thumbnail,,640,480")))))))


(define-bknr-tag one-image-of (&key keyword alt (width 648) (height 96) (background "000000"))
  (let ((image (random-elt (get-keyword-store-images (make-keyword-from-string keyword)))))
    (when image
      (html ((:img :src (image-url image :process #?"thumbnail,${background},${width},${height}") :alt alt :width width :height height))))))

(define-bknr-tag keywords-union (&key keywords)
  "Outputs an image collection containing the union of the images marked with the given keywords

Arguments: keywords (required) contains a comma-separated list of keywords

Output: A table with 5 images per row containing the thumbnails of images marked with one or more
of the keywords given in the argument keywords. The table is generated with the image-collection
function

Example:
<bknr:keywords-union keywords=\"robots,cards\" />

outputs

<div class=\"images\"><table><tr><td width=\"110\"><a href=\"/browse-image/3\" alt=\"peecol8\">
<img src=\"/image/3/thumbnail\" alt=\"peecol8\"/></a>
</td>
<td width=\"110\"><a href=\"/browse-image/4\" alt=\"peecol7\">
<img src=\"/image/4/thumbnail\" alt=\"peecol7\"/></a>
</td>
<td width=\"110\"><a href=\"/browse-image/37\" alt=\"EBY_dynasitelogo\">
<img src=\"/image/37/thumbnail\" alt=\"EBY_dynasitelogo\"/></a>
</td>
</tr>
</table>
</div>
"
  (let ((keys (when keywords (mapcar #'make-keyword-from-string (split "," keywords)))))
    (when keys
      (layouter (get-keywords-union-store-images keys)))))

(define-bknr-tag keywords-intersection (&key keywords)
  "Outputs an image collection containing the intersection of the images marked with the given keywords

Usage is the same as keywords-union, except that result is a collection of the images marked with
all the keywords given
"
  (let ((keys (when keywords (mapcar #'(lambda (k) (make-keyword-from-string (string-upcase k)))
					(split "," keywords)))))
    (when keys
      (image-collection (get-keywords-intersection-store-images keys)))))

(define-bknr-tag keyword-images (&key keyword)
  "List the images marked with the keyword arguments as an image collection

Arguments: keyword (required) the keyword

Output: a collection of the images marked with the given keyword.

Example:
<bknr:keyword keyword=\"robots\"/> would show the images marked with the keyword robot
"
  (when keyword
    (image-collection (sort (get-keyword-store-images (make-keyword-from-string keyword))
			    #'< :key #'store-image-aspect-ratio)
		      :title (format nil "Images with keyword ~a:" keyword))))

(define-bknr-tag image-keyword-choose-dialog (&key (size "2") (name "keyword") (create nil))
  "Outputs one or more keyword choose dialogs, which can be used to choose from
the list of available keywords or to add new keywords.

Arguments:
size (optional, default 4) the number of choose dialogs
name (optional, default \"keyword\") the name of the input fields which are output
create (optional, default nil) if a text-field for a new keyword should be output

Example:
<bknr:keyword-choose-dialog size=\"1\" name=\"foo\" create=\"t\" />

outputs

<div class=\"keyword-choose\"><select name=\"foo\" size=1><option value=\"\"></option>
<option value=\"LOGO\">LOGO</option>
<option value=\"PEECOL-TOP\">PEECOL-TOP</option>
</select>
<input type=\"text\" length=\"20\" name=\"foo\"/></div>

<bknr:keyword-choose-dialog size=\"4\" name=\"test\" />

outputs

<div class=\"keyword-choose\">1. <select name=\"test\" size=1><option value=\"\"></option>
<option value=\"LOGO\">LOGO</option>
<option value=\"PEECOL-TOP\">PEECOL-TOP</option>
</select>
</div>
<div class=\"keyword-choose\">2. <select name=\"test\" size=1><option value=\"\"></option>

<option value=\"LOGO\">LOGO</option>
<option value=\"PEECOL-TOP\">PEECOL-TOP</option>
</select>
</div>
<div class=\"keyword-choose\">3. <select name=\"test\" size=1><option value=\"\"></option>
<option value=\"LOGO\">LOGO</option>
<option value=\"PEECOL-TOP\">PEECOL-TOP</option>
</select>
</div>
<div class=\"keyword-choose\">4. <select name=\"test\" size=1><option value=\"\"></option>
<option value=\"LOGO\">LOGO</option>

<option value=\"PEECOL-TOP\">PEECOL-TOP</option>
</select>
</div>

"
  (let ((size (or (parse-integer size :junk-allowed t) 1)))
    (keyword-choose-dialog (all-image-keywords)
			   :size size :name name :create create)))

