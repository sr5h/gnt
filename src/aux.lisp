;;;; aux.lisp

(in-package #:gnt)

(defun insert-as-sorted-incr (item seq &optional (key #'identity))
  (funcall (insert :test #'<= :key key :duplicate nil) item seq))

(defun insert-as-sorted-decr (item seq &optional (key #'identity))
  (funcall (insert :test #'>= :key key :duplicate nil) item seq))

(defun insert (&key (key #'identity) (test #'eql) (duplicate t) (read nil))
  #'(lambda (item seq)
      (cond ((null seq) (append read (list item)))
	    ((and (not duplicate) (member item seq)) seq)
	    ((funcall test (funcall key item) (funcall key (car seq)))
	     (append read (cons item seq)))
	    (t
	     (funcall (insert :key key
			      :test test
			      :duplicate duplicate
			      :read (append read (list (car seq))))
		      item (cdr seq))))))

(defun select (&key (key #'identity) (test #'<) (combine-fn #'+) (acc 0.0)
		 (treat-nil #'(lambda ()
				nil)))
  #'(lambda (test-value seq)
      (cond ((null seq) (funcall treat-nil))
	    (t
	     (let ((new (funcall combine-fn acc (funcall key (car seq)))))
	       (cond ((funcall test test-value new) (car seq))
		     (t
		      (funcall (select :key key
				       :test test
				       :combine-fn combine-fn
				       :acc new
				       :treat-nil treat-nil)
			       test-value (cdr seq)))))))))
