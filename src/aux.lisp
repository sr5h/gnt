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

(defun select (&key (key #'identity) (test #'<) (update-fn (memo-random-float)) (combine-fn #'+) (acc 0.0)
		 (treat-nil #'(lambda ()
				nil)))
  #'(lambda (seq)
      (cond ((null seq) (funcall treat-nil))
	    (t
	     (let ((new (funcall combine-fn acc (funcall key (car seq)))))
	       (cond ((funcall test (funcall update-fn) new) (car seq))
		     (t
		      (funcall (select :key key
				       :test test
				       :update-fn update-fn
				       :combine-fn combine-fn
				       :acc new
				       :treat-nil treat-nil)
			       (cdr seq)))))))))

(defun memo-random (max)
  (let ((before nil))			; only 1 called
    #'(lambda ()
	(print before)
	(if before
	    before
	    (setf before (random max))))))

(defun memo-random-float ()
  (memo-random 1.0))

(defun sr-format (p &rest args)
  (apply (%sr-format :postfix p) args))

(defun %sr-format (&key (t-or-nil t) (prefix "") (postfix "") (acc nil))
  #'(lambda (&rest args)
      (cond ((null args) (apply #'format t-or-nil (concatenate 'string prefix
							   (concatenate 'string postfix
									"~%"))
				acc))
	    (t
	     (apply (%sr-format :t-or-nil t-or-nil
				 :prefix prefix
				 :postfix (concatenate 'string
						       postfix
						       "~a ")
				 :acc (append acc (list (car args))))
		    (cdr args))))))
