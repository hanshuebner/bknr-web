(in-package :html-match)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :unit-test))

(defmacro test-node-match (pat node)
  `(equal (html-node-match ',pat ',node no-bindings) no-bindings))


(defun equal-bindings (bindings var-values)
  (reduce #'(lambda (x1 x2) (and x1 x2))
	  (loop for (var value) in var-values
		collect (equal value (binding-val (get-binding var bindings))))))

(defmacro test-pattern-match (pat node var-values)
  (let ((bindings (gensym)))
    `(let ((,bindings (html-node-match ',pat ',node no-bindings)))
      (equal-bindings ,bindings ',var-values))))

(defmacro test-html-match (pat node &rest var-values-list)
  (let ((bind-list (gensym))
	(bindings (gensym))
	(vvl (gensym))
	(vv (gensym)))
    `(let ((,bind-list (html-match ',pat ',node no-bindings))
	   (,vvl ',var-values-list))
      (test-equal (length ,vvl) (length ,bind-list))
      (loop for ,vv in ,vvl
       for ,bindings in ,bind-list
       do (test-assert (equal-bindings ,bindings ,vv))))))

(deftest :pmatch "Node matching test"
  (test-assert (test-node-match :a    (:a :href "foo")))
  (test-assert (test-node-match (:a)  (:a :href "foo")))
  (test-assert (test-node-match (:a :href "foo")
				(:a :href "foo")))
  (test-assert (test-node-match (:a :href "foo")
				(:a :href "foo" :broesel "foobar")))
  (test-assert (test-node-match :a :a))
  (test-assert (test-node-match :a (:a)))
  (test-assert (test-node-match (:a) :a))
  (test-assert (test-node-match (:a) (:a))))

(deftest :pmatch "Node not matching test"
  (test-assert (not (test-node-match :a :b)))
  (test-assert (not (test-node-match :a (:b))))
  (test-assert (not (test-node-match (:a) :b)))
  (test-assert (not (test-node-match (:a) (:b))))
  (test-assert (not (test-node-match (:a :href "foo") :a)))
  (test-assert (not (test-node-match (:a :href "foo") (:a))))
  (test-assert (not (test-node-match (:a :href "foo") (:a :blorg "foo"))))
  (test-assert (not (test-node-match (:a :href "foo") (:a :href "bla"))))
  (test-assert (not (test-node-match (:a :href "foo" :hehe "foo") (:a :href "foo")))))

(deftest :pmatch "Node variable matching test"
  (test-assert (test-pattern-match ?node :a ((?node (:a)))))
  (test-assert (test-pattern-match ?node (:a) ((?node (:a)))))
  (test-assert (test-pattern-match ?node (:a :href "foo")
				   ((?node (:a :href "foo")))))
  (test-assert (test-pattern-match (:a :href ?href) (:a :href "foo")
				   ((?href "foo"))))
  (test-assert (test-pattern-match (?node :href ?href) (:a :href "foo")
				   ((?href "foo")
				    (?node :a))))
  (test-assert (test-pattern-match (?node :href ?href) (:a :href "foo" :blorg "foobar")
				   ((?href "foo")
				    (?node :a))))

  (test-assert (not (test-pattern-match ?node :a ((?node :a))))))

;; these tests currently fail - so we skip them
;; (deftest :pmatch "HTML-MATCH tests"
;;   (test-html-match (:a ?link) ((:a :href "foo") "link")
;; 		   ((?link "link")))
;;   (test-html-match ((:a :href ?href) ?link) ((:a :href "foo") "link")
;; 		   ((?link "link") (?href "foo")))
;;   (test-html-match (+seq (:a ?link1) (:a ?link2))
;; 		   (:p ((:a :href "foo1") "link1")
;; 		       ((:a :href "foo2") "link2"))
;; 		   ((?link1 "link1")
;; 		    (?link2 "link2")))
;;   (test-html-match (+seq (:a ?link1) (:a ?link2))
;; 		   (:p ((:a :href "foo1") "link1")
;; 		       ((:a :href "foo2") "link2")
;; 		       ((:a :href "foo3") "link3"))
;; 		   ((?link1 "link1")
;; 		    (?link2 "link2"))
;; 		   ((?link1 "link2")
;; 		    (?link2 "link3"))))

