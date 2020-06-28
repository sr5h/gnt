;;;; aux.lisp

(in-package #:gnt)

(defun insert-as-sorted-incr (item seq key)
  (funcall (insert :test #'<= :key key) item seq))

(defun insert-as-sorted-decr (item seq key)
  (funcall (insert :test #'>= :key key) item seq))

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
