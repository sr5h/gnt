;;;; gnt.lisp

(in-package #:gnt)

(defun gnt (population goal-p-fn)
  (cond ((null population) (error "ERROR: starved the population"))
	;; assert the population to be sorted.
	((funcall goal-p-fn (first population)) (first population))
	(t
	 (gnt (generate-next population) goal-p-fn))))

(defstruct indv
  chromosomes				; :thinkit: homologous or analogous?
  fitness				; :thinkit: best, average or sum?
  )

(defun generate-next (population
		      &key
			(p-combi-fn) (select-fn) (meiosis-1-fn) (meiosis-2-fn) (combi-fn) (set-fitness-fn)
			(max (length population)) (c 0) (new-population nil) (selected-parents))
  (cond ((= max c)
	 ;; sorted new-population and selected-parents.
	 ;; and result of combine-fn is sorted list.
	 (funcall combi-fn new-population selected-parents))
	(t
	 (let ((p1 (funcall select-fn (random 1.0) population)) ;:thinkit:
	       (p2 (funcall select-fn (random 1.0) population)))
	   (let ((m-indv1 (meiosis (list p1) meiosis-1-fn meiosis-2-fn))
		 (m-indv2 (meiosis (list p2) meiosis-1-fn meiosis-2-fn)))
	     (generate-next population
			    :p-combi-fn p-combi-fn
			    :select-fn select-fn
			    :meiosis-1-fn meiosis-1-fn
			    :meiosis-2-fn meiosis-2-fn
			    :combi-fn combi-fn
			    :set-fitness-fn set-fitness-fn

			    :max max
			    :c (1+ c)
			    :new-population
			    (insert-as-sorted-decr (make-indv :chromosomes
							      (funcall combi-fn m-indv1 m-indv2)
							      :fitness
							      (funcall set-fitness-fn m-indv1 m-indv2))
						   new-population)
			    :selected-parents
			    (insert-as-sorted-decr p2 (insert-as-sorted-decr p1 selected-parents))))))))


;; return: indvs is a list
(defun meiosis (indvs meiosis-1-fn meiosis-2-fn)
  (funcall meiosis-2-fn
	   (funcall meiosis-1-fn indvs)))

(defun %meiosis (get-chromosomes-fn double-fn crossover-fn split-fn &optional (meiosised-indvs nil))
  #'(lambda (indvs)
      (cond ((null indvs) meiosised-indvs)
	    (t
	     (funcall
	      (%meiosis get-chromosomes-fn double-fn crossover-fn split-fn
			(append meiosised-indvs
				(funcall split-fn
					 (funcall crossover-fn
						  (funcall double-fn
							   (funcall get-chromosomes-fn (car indvs)))))))
	      (cdr indvs))))))
