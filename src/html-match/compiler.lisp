(in-package :html-match)


;; helpers

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym ,(symbol-name s))))
		 syms)
    ,@body))


(defvar *traced* nil)
(defvar *trace-level* 0)
(defvar *traceall* nil)

(defmacro hpc-trace (&rest names)
  `(progn (dolist (name ',names)
	    (pushnew name *traced*))
    *traced*))

(defun hpc-untrace (&rest names)
  (if (null names)
      (setf *traced* nil)
      (setf *traced* (set-difference *traced* names))))

(defun trace-entry (name pattern &rest args)
  (when (or *traceall*
	    (member name *traced*))
    (let ((*print-length* debug::*debug-print-length*)
	  (*print-level* debug::*debug-print-level*))
    (format t "~A[~D] ~A ~S ~{~S~^, ~}~%"
	    (make-string (* 3 *trace-level*) :initial-element #\Space)
	    *trace-level* name pattern args))))

(defun trace-output (name output)
  (when (or *traceall*
	    (member name *traced*))
    (let ((*print-length* debug::*debug-print-length*)
	  (*print-level* debug::*debug-print-level*))
      (format t "~A[~D] ~A => ~S~%" (make-string (* 3 *trace-level*) :initial-element #\Space)
	      *trace-level* name output))))

(defmacro trace-point (name (pattern &rest args) &rest resform)
  ;; das versteh ich jetzt wirklich nicht, aber hmm?
  (with-gensyms (res)
    ``(progn
       (trace-entry ',',name ',,pattern ,,@args)
       (let ((,',res (let ((*trace-level* (1+ *trace-level*))) ,,@resform)))
	 (trace-output ',',name ,',res)
	 ,',res))))

(defmacro defhpc (name (pattern &rest args) &rest body)
  #+nil
  `(defun ,name (,pattern ,@args)
    (trace-point ,name (,pattern ,@args)
     ,@body))
  `(defun ,name (,pattern ,@args)
    ,@body))

;; pattern functions

(defun binding-pattern-p (pattern)
  (cond ((regex-pattern-p pattern) t)
	((and (variable-p pattern)
	      (not (ignore-p pattern))) t)
	((consp pattern)
	 (some #'binding-pattern-p pattern))
	(t niL)))

(defun static-pattern-p (pattern)
  (not (binding-pattern-p pattern)))

(defun node-pattern-p (pattern)
  (or (keywordp pattern)
      (and (consp pattern)
	   (keywordp (first pattern)))))

;; static patterns, return NIL or T

(defhpc hpc-string (pattern input)
  `(and (stringp ,input) (string-equal ,pattern ,input)))

(defhpc hpc-static-node (pattern input)
  (let* ((pat (if (atom pattern) (list pattern) pattern))
	 (pat-name (car pat))
	 (pat-attrs (cdr pat)))
    #+nil
    (format t "pat ~S, name ~S, attrs ~S~%" pat pat-name pat-attrs)
    (with-gensyms (node-attrs)
      `(if (atom ,input)
	,(if pat-attrs
	      nil
	      `(eql ,pat-name ,input))
	,(if pat-attrs
	     `(and (eql (first ,input) ,pat-name)
	       (let ((,node-attrs (cdr ,input)))
		 ,@(loop for (attr val) on pat-attrs by #'cddr
			 for node-val = `(getf ,node-attrs ,attr)
			 when (ignore-p val)
			 collect `(stringp ,node-val)
			 else do (unless (stringp val)
				   (error "attribute value ~A is not a string" val))
			 collecting `(string-equal ,node-val ,val))))
	     `(eql (first ,input) ,pat-name))))))

(defhpc hpc-static-segment (pattern input)
  `(and ,@(loop for cdrcnt from 0
		for pat in pattern
		unless (segment-pattern-p pat)
		collect (hpc-static pat `(car (nthcdr ,cdrcnt ,input)))
		else collect (hpc-static pat `(nthcdr ,cdrcnt ,input)))))

(defhpc hpc-static (pattern input)
  (cond ((stringp pattern) (hpc-string pattern input))

	((ignore-p pattern) t)

	((segment-pattern-p pattern)
	 (hpc-static-segment pattern input))
	
	((consp pattern)
	 (if (and (= (length pattern) 1)
		  (or (ignore-p (first pattern))
		      (stringp (first pattern))))
	     (if (ignore-p (first pattern))
		 t
		 `(if (consp ,input)
		   (eql ,(first pattern) (first ,input))
		   (eql ,(first pattern) ,input)))
	     (with-gensyms (head children)
	       `(and (consp ,input)
		 (let ((,head (car ,input))
		       (,children (cdr ,input)))
		   (and ,(hpc-static-node (first pattern) head)
			,(hpc-static-segment (cdr pattern) children)))))))
	
	(t (error "Unknown static pattern ~A~%" pattern))))

;; simple binding patterns, return bindings

(defhpc hpc-variable (variable input bindings)
  `(extend-bindings ',variable ,input ,bindings))

(defun hp-regex-match (regex input variables bindings)
  (unless (stringp input)
    (return-from hp-regex-match fail))
  (multiple-value-bind (start end reg-start reg-end)
      (scan regex input)
    (declare (ignore end))
    (if start
	(regex-match-variables input reg-start reg-end variables bindings)
	fail)))

(defun regex-match-variables (input reg-start reg-end variables bindings)
  (loop for var in variables
	for match from 0 below (length reg-start)
	do (setf bindings (match-variable var (subseq input
						      (aref reg-start match)
						      (aref reg-end   match))
					  bindings))
	until (eql bindings fail)
	finally (return bindings)))

(defhpc hpc-regex (pattern input bindings)
  (let ((regex (regex-pattern-regex pattern))
	(variables (regex-pattern-variables pattern)))
    `(hp-regex-match ,regex ,input ',variables ,bindings)))

(defhpc hpc-node-attrs (pat-attrs node-attrs bindings)
  (when (null pat-attrs)
    (error "empty pattern attributes"))
  
  (let ((static-patterns (loop for pat on pat-attrs by #'cddr
			       unless (binding-pattern-p pat)
			       collect pat))
	(binding-patterns (loop for pat on pat-attrs by #'cddr
			       when (binding-pattern-p pat)
			       collect pat)))

    (format t "static patterns: ~S, bindings patterns ~S~%"
	    static-patterns binding-patterns)
    
    (flet ((hpc-binding-patterns ()
	     (with-gensyms (block)
	       `(block ,block
		 ,@(loop for (attr val) in binding-patterns
			 for node-val = `(getf ,node-attrs ,attr)
			 appending `((setf ,bindings ,(hpc val node-val bindings))
				   (when (eql ,bindings fail)
				     (return-from ,block fail))))
		 ,bindings))))

      (if static-patterns
	  `(when (and ,@(loop for (attr val) in static-patterns
			      for node-val = `(getf ,node-attrs ,attr)
			      do (unless (stringp val)
				   (error "attribute value ~A is not a string" val))
			      collecting `(string-equal ,node-val ,val)))
	    ,(if binding-patterns
		 (hpc-binding-patterns)
		 bindings))
	  (hpc-binding-patterns)))))
	
(defhpc hpc-node (pattern input bindings)
  (let* ((pat (if (atom pattern) (list pattern) pattern))
	 (pat-name (car pat))
	 (pat-attrs (cdr pat)))
    (if (null pat-attrs)
	(if (variable-p pat-name)
	    (hpc-variable pat-name input bindings)
	    (error "Static pattern compiled as dynamic: ~A" pattern))

	(with-gensyms (node-name node-attrs)
	  `(if (atom ,input)
	       fail
	    (let* ((,node-name (car ,input))
		   (,node-attrs (cdr ,input)))
	      ,(if (variable-p pat-name)
		   (with-gensyms (new-bindings)
		     `(let ((,new-bindings ,(hpc-variable  pat-name node-name bindings)))
		       ,(hpc-node-attrs pat-attrs node-attrs new-bindings)))
		   `(if (eql ,pat-name ,node-name)
		     ,(hpc-node-attrs pat-attrs node-attrs bindings)
		     fail))))))))


;; extended binding patterns, return a list of bindings

(defhpc hpc-segment-pattern (pattern input bindings)
  (with-gensyms (block)
    `(block ,block
      (unless (consp ,input)
	(return-from ,block fail))
      (when (< (length ,input) ,(1- (length pattern)))
	(return-from ,block fail))

      
      ,@(loop for patterns on (cdr pattern)
	      for pat = (car patterns)
	      collecting (cond ((static-pattern-p pat)
				(with-gensyms (in)
				  `(let ((,in (car ,input)))
				    (unless ,(hpc-static pat in)
				      (return-from ,block fail)))))

			       ((and (variable-p pat)
				     (= (length patterns) 1))
				`(setf ,bindings
				  ,(hpc-variable pat input bindings)))

			       (t
				`(setf ,bindings
				  ,(with-gensyms (in)
				     `(let ((,in (car ,input)))

				       ,(hpc pat in bindings))))))

	      unless (= (length patterns) 1)
	      collect `(setf ,input (cdr ,input)))
      
      ,bindings)))

(defhpc hpc-segment (pattern input bindings)
  (with-gensyms (block)
    `(block ,block
      (unless (consp ,input)
	(return-from ,block fail))
      ,(hpc-segment-pattern pattern input bindings))))

(defhpc hpc (pattern input bindings)
  (cond ((static-pattern-p pattern)
	 `(when ,(hpc-static pattern input)
	   ,bindings))

	((regex-pattern-p pattern)
	 (hpc-regex pattern input bindings))
	
	((ignore-p pattern) bindings)

	((variable-p pattern)
	 (hpc-variable pattern input bindings))

	((segment-pattern-p pattern)
	 (with-gensyms (in binds)
	   `(if (consp ,input)
	     (do ((,in (cdr ,input) (cdr ,in))
		  (,binds fail))
		 ((or (null ,in)
		      (not (eql ,binds fail))) ,binds)
	       (setf ,binds ,(hpc-segment-pattern pattern in bindings))))))

	((and (consp pattern)
	      (node-pattern-p (first pattern)))
	 (let ((patnode (first pattern))
	       (patsegment (cdr pattern)))
	   (with-gensyms (block node children)
	     `(block ,block
	       (let* ((,node (if (consp ,input) (car ,input) ,input))
		      (,children (if (consp ,input) (cdr ,input) nil)))

		 ,@(when (null patsegment)
			 `(unless (null, children)
			   (return-from ,block fail)))
		 ,(cond ((static-pattern-p patnode)
			 `(unless ,(hpc-static-node patnode node)
			   (return-from ,block fail)))

			(t `(when (eql (setf ,bindings ,(hpc-node patnode node bindings))
				   fail)
			     (return-from ,block fail))))
		 
		 ,(hpc-segment-pattern pattern children bindings))))))

	(t (error "form not supported"))))

(defun hpc-pattern-lambda (pattern)
  (with-gensyms (input bindings)
    `(lambda (,input ,bindings)
      (declare (optimize (speed 2) (safety 0)))
      ,(hpc pattern input bindings))))

(defmacro hpc-pattern (pattern &rest body)
  (let ((bindings (gensym))
	(vars (pattern-vars pattern)))
   `(list ,(hpc-pattern-lambda pattern)
     #'(lambda (,bindings)
	 ,@(pattern-callback-walker body bindings vars)))))

(defun hpc-patmatch (input &rest hpc-patterns)
  (dolist (hpc-pattern hpc-patterns)
    (let* ((patfun (first hpc-pattern))
	   (callback (second hpc-pattern))
	   (bindings (funcall patfun input no-bindings)))
      (unless (eql bindings fail)
	(funcall callback bindings)))))

(defmacro hpc-match (pattern input)
  (with-gensyms (in bindings)
    `(let ((,bindings no-bindings)
	   (,in ,input))
      ,(hpc pattern in bindings))))

(defun hpc-search (input &rest hpc-patterns)
  (apply #'hpc-patmatch input hpc-patterns)
  (when (consp input)
    (let ((*html-parent* input))
      (dolist (child (rest input))
        (apply #'hpc-search child hpc-patterns)))))

(defparameter *pat*
  (hpc-pattern
      (+seq
       (:comment " figures starts")
       (:tr ?_)
       (:tr (:td (:table ?figures))))
    (hpc-search ?figures
      (hpc-pattern
	  (:tr (:td (:img))
	       (:td ((:a :href ?href) ?name)))
	(let* ((fid (subseq ?href 22 30)))
	  (format t "figure ~A~%" fid))))))

(defun hpc-test (input)
  (hpc-search input
    (hpc-pattern (:td ((:font :class "authorname") ?author)
		      ((:font :class "authorinstitute") ?institute))
      (format t "author ~S, institute ~S~%" ?author ?institute))

    (hpc-pattern
     ((:font :class "articletype") ?type)
      (format t "type ~S~%" ?type))

    ;; Title
    (hpc-pattern
     ((:td :class "articletitle") (:a) ?title)
      (format t "title ~S~%" ?title))

    ;; definition
    (hpc-pattern
     ((:p :class "definition") ?definition)
      (format t "definition ~S~%" ?definition))

    ;; figures
    (hpc-pattern
	(+seq
	 (:comment " figures starts")
	 (:tr ?_)
	 (:tr (:td (:table ?figures))))
      
      (hpc-search ?figures
	(hpc-pattern
	    (:tr (:td (:img))
		 (:td ((:a :href ?href) ?name)))
	  (let* ((fid (subseq ?href 22 30)))
	    (format t "figure ~A~%" fid)))))

      ;; tables
    (hpc-pattern
	(+seq
	 (:comment " Table starts")
	 (:tr ?_)
	 (:tr (:td (:table ?tables))))
      (hpc-search ?tables
	(hpc-pattern
	    (:tr (:td (:img))
		 (:td ((:a :href (+regex #?r"javascript:viewTable\('([^']*)'," ?tid)) ?name)))
	  (format t "table ~A~%" ?tid))))
    
    ;; section
    (hpc-pattern
     (:table (:tr ((:td :class "articletitle") ?_))
	     ?body)
      (format t "section~%"))

    ;; references
    (hpc-pattern
	((:td :class "references")
	 (:div ?reference))
      (format t "ref ~A~%" ?reference))

    ;; published
    (hpc-pattern
	((:td :class "orginalpub") (:b ?_)
	 (+regex #?r":.(.*)$" ?date) ?_)
      (format t "date: ~A~%" ?date))))
    

(defun html-test (input)
  (html-search input
    (html-pattern (:td ((:font :class "authorname") ?author)
		      ((:font :class "authorinstitute") ?institute))
      (format t "author ~S, institute ~S~%" ?author ?institute))

    (html-pattern
     ((:font :class "articletype") ?type)
      (format t "type ~S~%" ?type))

    ;; Title
    (html-pattern
     ((:td :class "articletitle") (:a) ?title)
      (format t "title ~S~%" ?title))

    ;; definition
    (html-pattern
     ((:p :class "definition") ?definition)
      (format t "definition ~S~%" ?definition))

    ;; figures
    (html-pattern
	(+seq
	 (:comment " figures starts")
	 (:tr ?_)
	 (:tr (:td (:table ?figures))))
      
      (html-search ?figures
	(html-pattern
	    (:tr (:td (:img))
		 (:td ((:a :href ?href) ?name)))
	  (let* ((fid (subseq ?href 22 30)))
	    (format t "figure ~A~%" fid)))))

      ;; tables
    (html-pattern
	(+seq
	 (:comment " Table starts")
	 (:tr ?_)
	 (:tr (:td (:table ?tables))))
      (html-search ?tables
	(html-pattern
	    (:tr (:td (:img))
		 (:td ((:a :href (+regex #?r"javascript:viewTable\('([^']*)'," ?tid)) ?name)))
	  (format t "table ~A~%" ?tid))))
    
    ;; section
    (html-pattern
     (:table (:tr ((:td :class "articletitle") ?_))
	     ?body)
      (format t "section~%"))

    ;; references
    (html-pattern
	((:td :class "references")
	 (:div ?reference))
      (format t "ref ~A~%" ?reference))

    ;; published
    (html-pattern
	((:td :class "orginalpub") (:b ?_)
	 (+regex #?r":.(.*)$" ?date) ?_)
      (format t "date: ~A~%" ?date))))
    
