(in-package :html-match)

(defconstant fail nil
  "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
  "Indicates pat-match success, with no variables")

(defun regex-pattern-p (pattern)
  (and (consp pattern)
       (eql (first pattern) '+regex)))

(defun regex-pattern-regex (pattern)
  (second pattern))

(defun regex-pattern-variables (pattern)
  (cddr pattern))

;;; variable handling
(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (or (and (symbolp x) (equal (char (symbol-name x) 0) #\?))
      (regex-pattern-p x)))

(defun get-binding (var bindings)
  "Find a (Variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single biding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single biding."
  (cdr binding))

(defun make-binding (var val)
  (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
	;; Once we add a "real" binding,
	;; we can get rid of the dummy no-bindings
	(if (eq bindings no-bindings)
	    nil
	    bindings)))

(defun match-regex-pattern (pattern input bindings)
  (let ((regex (regex-pattern-regex pattern))
	(variables (regex-pattern-variables pattern)))
    (multiple-value-bind (start end reg-start reg-end)
	(scan regex input)
      (declare (ignore end))
      (if start
	  (loop for var in variables
		for match from 0 below (length reg-start)
		do (setf bindings (match-variable var (subseq input (aref reg-start match)
							      (aref reg-end match))
						  bindings))
		until (eql bindings fail)
		finally (return bindings))
	  fail))))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (cond ((regex-pattern-p var)
	 (match-regex-pattern var input bindings))
	((ignore-p var)
	 bindings)
	(t (let ((binding (get-binding var bindings)))
	     (cond ((not binding) (extend-bindings var input bindings))
		   ((equal input (binding-val binding)) bindings)
		   (t fail))))))

(defvar *html-parent* nil)

(defun html-node-match (pat node bindings)
  (when (atom node)
    (setf node (list node)))
  
  (cond ((eq bindings fail)
	 fail)

	((variable-p pat)
	 (match-variable pat node bindings))

	(t (let* ((pat   (if (atom pat) (list pat) pat))
		  (pat-name   (car pat))
		  (node-name  (car node))
		  (pat-attrs  (cdr pat))
		  (node-attrs (cdr node))
		  (bindings   (if (variable-p pat-name)
				  (match-variable pat-name node-name bindings)
				  bindings)))
	     (if (or (variable-p pat-name)
		     (eql pat-name node-name))
		 (loop for (attr val) on pat-attrs by #'cddr
		       for node-val = (getf node-attrs attr)
		       do (cond ((variable-p val)
				 (setf bindings (match-variable val node-val bindings)))
				((not (string-equal val node-val))
				 (setf bindings fail)))
		       until (eq bindings fail)
		       finally (return bindings))
		 fail)))))

(defun segment-pattern-p (pattern)
  (and (consp pattern)
       (eql (first pattern) '+seq)))

(defun multiple-pattern-p (pattern)
  (and (consp pattern)
       (eql (first pattern) '+mul)))

(defun or-pattern-p (pattern)
  (and (consp pattern)
       (eql (first pattern) '+or)))

(defun ignore-p (pattern)
  (or (eql pattern '?_)
      (equal pattern '(?_))))

(defun html-match-segment (patterns trees bindings)
  (cond ((null bindings)
	 nil)
	((null patterns)
	 (list bindings))
	((and (= (length patterns) 1)
	      (variable-p (first patterns)))
	 (list (match-variable (first patterns) trees bindings)))
	(t (let ((binds-list (html-match (car patterns)
					 (car trees) bindings)))
	     (when binds-list
	       (mapcan #'(lambda (binds)
			   (html-match-segment (cdr patterns)
					       (cdr trees) binds))
		       binds-list))))))

(defun html-match (pattern tree &optional (bindings no-bindings))
  (cond ((eq bindings fail) nil)
	
	((ignore-p pattern) (list bindings))

	((eql pattern tree) (list bindings))

	;; greedy
	((variable-p pattern)
	 (list (match-variable pattern tree bindings)))
	
	((and (stringp pattern)
	      (stringp tree)
	      (string-equal tree pattern))
	 (list bindings))
	
	((or-pattern-p pattern)
	 (mapcan #'(lambda (pat) (html-match pat tree bindings))
		 (cdr pattern)))
	
	((and (segment-pattern-p pattern)
	      (consp tree))
	 (reduce #'nconc
		 (loop for tree on (cdr tree)
		       for binds = (html-match-segment (cdr pattern) tree bindings)
		       when (not (eq binds fail))
		       collecting binds)))

	((and (multiple-pattern-p pattern)
	      (consp tree))
	 (error "multiple pattern not supported for now"))
	
	((and (consp pattern) (consp tree))
	 (let ((bindings (html-node-match (first pattern)
					  (first tree) bindings)))
	   (html-match-segment (cdr pattern) (cdr tree) bindings)))
	
	(t nil)))

;; das koennte mal hygienischer sein
(defun pattern-vars (pattern)
  (cond ((regex-pattern-p pattern)
	 (copy-list (cddr pattern)))
	((variable-p pattern)
	 (list pattern))
	((consp pattern)
	 (union (pattern-vars (car pattern))
		(pattern-vars (cdr pattern))))
	(t nil)))

(defun pattern-callback-walker (tree binding-var pattern-vars)
  (cond ((and (variable-p tree)
	      (member tree pattern-vars))
	 `(binding-val (get-binding ',tree ,binding-var)))
	((atom tree) tree)
	((eql (car tree) 'html-pattern)
	  `(html-pattern ,(cadr tree)
	    ,@(pattern-callback-walker (cddr tree) binding-var
				       (set-difference pattern-vars
						       (pattern-vars (cadr tree))))))
	(t (let ((a (pattern-callback-walker (car tree) binding-var pattern-vars))
		 (d (pattern-callback-walker (cdr tree) binding-var pattern-vars)))
	     (if (and (eql a (car tree))
		      (eql d (cdr tree)))
		 tree
		 (cons a d))))))

(defmacro html-pattern (pattern &rest body)
  (let ((bindings (gensym))
	(vars (pattern-vars pattern)))
   `(list ',pattern
     #'(lambda (,bindings)
	 ,@(pattern-callback-walker body bindings vars)))))

(defun html-patmatch (input &rest html-patterns)
  (dolist (html-pattern html-patterns)
    (let* ((pattern (first html-pattern))
	   (callback (second html-pattern))
	   (bind-lists (html-match pattern input no-bindings)))
      (dolist (bindings bind-lists)
	(funcall callback bindings)))))

(defun html-search (input &rest html-patterns)
  (apply #'html-patmatch input html-patterns)
  (when (consp input)
    (let ((*html-parent* input))
      (dolist (child (rest input))
        (apply #'html-search child html-patterns)))))

(defun html-replace (input &rest html-patterns)
  (dolist (html-pattern html-patterns)
    (let ((bind-lists (html-match (first html-pattern) input no-bindings)))
      (when (> (length bind-lists) 2)
	(error "Can not replace a node when there is a multiple match"))
      (when bind-lists
	(return-from html-replace (funcall (second html-pattern) (first bind-lists))))))
  (if (consp input)
      (cons (car input)
	    (mapcar #'(lambda (x) (apply #'html-replace x html-patterns))
		    (rest input)))
      input))
  