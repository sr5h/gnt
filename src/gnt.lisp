;;;; gnt.lisp

(in-package #:gnt)

(defun gnt (population goal-p generate-next)
  (cond ((null population) (error "ERROR: starved the population"))
	((funcall goal-p (first population)) (first population))
	(t
	 (gnt (funcall generate-next population) goal-p generate-next))))
