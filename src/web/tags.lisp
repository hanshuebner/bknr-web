(in-package :bknr.web)

(enable-interpol-syntax)

(defvar *toplevel-children*)

(define-bknr-tag toplevel (&key title (template "toplevel"))
  (setf (get-template-var :title) title)
  (when (and (not (scan "^/" template))
	     (scan "/" (request-variable :template-path)))
    (setf template (concatenate 'string
				(regex-replace #?r"(.*/).*$" (request-variable :template-path) #?r"\1")
				template)))
  (let* ((expander *template-expander*)
         (pathname (find-template-pathname expander template))
         (toplevel (get-cached-template pathname expander))
         (*toplevel-children* *tag-children*))
    (emit-template-node *template-expander* toplevel)))

(define-bknr-tag tag-body ()
  (let ((*tag-children* *toplevel-children*))
    (emit-tag-children)))

(define-bknr-tag redirect-request (&key target)
  ;; target here is relative to the current request
  (redirect (princ-to-string (puri:merge-uris target (script-name*)))))

(define-bknr-tag select-box (name options &key (size 1) default)
  (html ((:select :name name :size size)
	 (unless default
	   (html ((:option :value ""))))
	 (loop for option in options
	       for option-name = (if (consp option) (car option) option)
	       for option-value = (if (consp option) (cadr option) option)
	       do (if (equal option-name default)
		      (html ((:option :value option-value :selected "selected")
			     (:princ-safe option-name)))
		      (html ((:option :value option-value)
			     (:princ-safe option-name))))))))

(define-bknr-tag submit-button (action text &key confirm formcheck)
  (declare (ignore text))
  (if (or confirm formcheck)
      (html ((:input :type "submit" :name "action" :value action
		     "onClick" (cond
				 (confirm (format nil "javascript:return confirm('~a');" confirm))
				 (formcheck formcheck)))))
      (html ((:input :type "submit" :name "action" :value action)))))

(define-bknr-tag text-field (name &key (size 50) value)
  (if value
      (html ((:input :type "text" :size size :name name :value value)))
      (html ((:input :type "text" :size size :name name)))))

(define-bknr-tag textarea-field (name &key (cols 60) (rows 15) value)
  (html ((:textarea :name name :cols cols :rows rows)
	 (if value (html (:princ-safe value)) (html " ")))))

(define-bknr-tag checkbox-field (name value &key (checked nil))
  (if checked
      (html ((:input :type "checkbox" :name name :checked "checked") (:princ-safe value)))
      (html ((:input :type "checkbox" :name name) (:princ-safe value)))))

(define-bknr-tag date-field (name &key date (show-time t))
  "Generate a date entry widget using HTML <select> elements."
  (unless date
    (setf date (get-universal-time)))
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time date)
    (declare (ignore sec))
    (html ((:div :class "date")
	   (select-box (format nil "~a-day" name)
		       (genlist 1 31)
		       :default day) " / "
	   (select-box (format nil "~a-month" name)
		       (genlist 1 12)
		       :default month) " / "
	   (select-box (format nil "~a-year" name)
		       (genlist (- year 20) (+ year 20))
		       :default year) " "
	   (when show-time
	     (select-box (format nil "~a-hour" name)
			 (genlist 0 23)
			 :default hour)
	     (select-box (format nil "~a-minute" name)
			 (genlist 0 59)
			 :default min))))))

(define-bknr-tag keyword-choose-dialog (keywords &key (size 4) (name "keyword")
						 (create nil) (values nil)
						 (empty t))
  (setf keywords (union keywords values))
  (dotimes (i size)
    do (html ((:div :class "keyword-choose")
	      (when (> size 1)
		(html (:princ-safe i) ". "))
	      (select-box name
			  (loop for keyword in (if empty
						   (cons :|| (sort (copy-list keywords)
								   #'string<))
						   (sort (copy-list keywords) #'string<))
				collect (list (string-downcase keyword) keyword))
			  :default (string-downcase (symbol-name (nth i values))))
	      (when create
		(html ((:input :type "text" :length "20" :name name))))))))

#+(or)
(define-bknr-tag template-sourcecode (&key)
  "Shows the source code of the current template"
  (with-open-file (s bknr.web::*current-template-pathname* :direction :input)
    (html (:pre (loop for line = (read-line s nil 'eof)
		      until (eq line 'eof)
		      do (html (:princ-safe line) :newline))))))

(defun user-template-prefix ()
  (error 'wtf)
  (user-preference (bknr-session-user) :template-path-prefix "file:///Volumes/web/template/"))

;; xxx new templater
#+(or)
(define-bknr-tag template-url (&key (base (user-template-prefix)))
  "Gives an URL to template file"
  (html (cmslink (concatenate 'string base (pathname-name bknr.web::*current-template-pathname*) ".bknr")
	 "Edit the template " (:princ-safe (pathname-name bknr.web::*current-template-pathname*)) ".bknr")))

;; xxx new templater
#+(or)
(define-bknr-tag template-directory (&key (base (user-template-prefix)))
  "Gives an URL to the directory containing the template file"
  (html (cmslink (concatenate 'string base
			      (namestring (make-pathname
					   :directory (pathname-directory bknr.web::*current-template-pathname*))))
	  "Open the directory containing " (:princ-safe (pathname-name bknr.web::*current-template-pathname*))
	 ".bknr")))

(define-bknr-tag logo ()
  "Outputs the dynasite logo and the title of the page.

Output: the site logo

Example:
<bknr:logo />

"
  (html ((:img :style (parenscript:css-inline :float "right") :src (website-site-logo-url *website*) :alt "logo"))))
	
(define-bknr-tag body-style (&key background-color background-image)
  "Outputs the body style css definition

Arguments:      background-color (optional, background color for body css element)
		background-image (optional, background image for body css element, name of an internal image)

Output: <style> element

Example
<bknr:body-style background-color=\"99CCCC\" background-image=\"camuback_square_11\" />

outputs:

<style> body { background-image=url(/image/camuback_square_11; background-color=#99cccc; } </style>
"
  (html
   ((:style :type "text/css")
    #?"body { "
    (when background-image
      (html (:princ-safe #?"background-image: url(/image/$(background-image));")))
    (when background-color
      (html (:princ-safe #?"background-color: #$(background-color);")))
    "}")))

(define-bknr-tag header (&key title)
  "Outputs the dynasite header containing the title for the page and a reference to the static css files

Arguments: title (optional, default is \"untitled\")

Output: the html head title and a link to the stylesheet

Example
<bknr:header title=\"test\" />

outputs:

<head>
<link rel=\"stylesheet\" href=\"/static/css/dynastyle_01.css\" ....
"
  (html
   #+(or) ((:base :href (website-base-href *website*)))
   (loop for stylesheet in (website-style-sheet-urls *website*)
	 do (html ((:link :rel "stylesheet" :type "text/css" :href stylesheet))))
   (loop for javascript in (website-javascript-urls *website*)
	 do (html ((:script :type "text/javascript"
			    :language "JavaScript" :src javascript) "")))
   (when (website-rss-feed-url *website*)
     (html
      ((:link :rel "alternate"
	      :type "application/rss+xml"
	      :title "RSS Feed"
	      :href (website-rss-feed-url *website*)))))
   ((:link :rel "shortcut icon" :href "/favicon.ico" :type "image/png"))
   ((:meta :http-equiv "content-type" :content "text/html;charset=utf-8"))
   (when title
     (html (:title (:princ-safe title))))))

(define-bknr-tag navi-button (&key url text)
  (html (:princ "&nbsp;"))
  (if (equal (script-name*)
	     url)
      (html (:princ-safe text))
      (html (cmslink url (:princ-safe text))))
  (html (:princ "&nbsp;")))

(define-bknr-tag navigation (&key)
  "Outputs the navigation bar for dynasite

Output: a paragraph of class \"navi\" containing the navigation buttons

Example:
<bknr:navigation />

outputs:

<p class=\"navi\"><a href=\"/all-images\">&nbsp;all-images&nbsp;</a>
...
</p>
"
  (when (website-navigation *website*)
    (html
     ((:div :class "navi")
      (loop
       for (name . link) in (website-navigation *website*)
       do (navi-button :url link
		       :text name)))))
  (when (and (website-admin-navigation *website*)
	     (admin-p (bknr-session-user)))
    (html ((:div :class "navi")
	   "admin: "
	   (loop
	    for (name . link) in (website-admin-navigation *website*)
	    do (navi-button :url link
			    :text name))))))

(define-bknr-tag session-info ()
  (website-session-info *website*))

(defun make-html-menu (menu &key current-menu current-submenu (ul-id "navlist") (active-id "active") (link-prefix "/"))
  `((:ul :id ,ul-id)
    ,@(loop for choice in menu
	    collect `((:li ,@(when (string-equal current-menu (choice-link choice)) (list :class active-id)))
		      (cmslink ,(format nil "~a~a" link-prefix (choice-link choice)) ,(choice-title choice)))
	    when (and (choice-submenu choice)
		      (string-equal current-menu (choice-link choice)))
	    collect (make-html-menu (choice-submenu choice)
				    :current-menu current-submenu
				    :current-submenu nil
				    :link-prefix (format nil "/~a/" (choice-link choice))
				    :ul-id "subnavlist"
				    :active-id "subactive"))))

#+(or)
(define-bknr-tag site-menu ()
  (destructuring-bind
	(empty first-level &optional second-level &rest rest)
      (split "/" (script-name*))
    (declare (ignore empty rest))
    (html ((:div :id "navcontainer")
	   (let ((*standard-output* *html-stream*))
	     (emit-html
	      (make-html-menu (website-menu *website*)
			      :current-menu first-level
			      :current-submenu second-level)))))))

(define-bknr-tag date-url (url date &key name)
  (html (cmslink (format nil "~a?date=~a" url date)
	  (:princ-safe 
	   (if name
	       name
	       (format-date-time date :show-time nil
				 :show-seconds nil
				 :show-weekday t))))))

(define-bknr-tag next-days-list (url &key (start (get-universal-time))
				     (count 5))
  (let* ((current-date (get-universal-time))
	 (next-days (loop for i from count downto 1
			  for date = (next-day i :start start)
			  unless (> date current-date)
			  collect date)))
    (when next-days
      (html (:ul
	     (when (= (length next-days) count)
	       (html (:li (date-url url (pop next-days) :name "later"))))
	     (dolist (day next-days)
	       (html (:li (date-url url day)))))))))

(define-bknr-tag previous-days-list (url &key (start (get-universal-time))
					 (count 5))
  (let ((previous-days (loop for i from 1 to (1- count)
			     for date = (previous-day i :start start)
			     collect date))
	(last-day (previous-day count :start start)))
    (html (:ul (dolist (day previous-days)
		 (html (:li (date-url url day))))
	       (:li (date-url url last-day :name "earlier"))))))

