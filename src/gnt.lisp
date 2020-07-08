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
			(fitness-fn #'(lambda (i) (slot-value i 'fitness)))
			(p-combi-fn)
			(select-fn)
			(meiosis-1-fn)
			(meiosis-2-fn)
			(select-m-indvs-fn (select-m-indvs fitness-fn))
			(combi-fn #'(lambda (i1 i2) ; not sorted, which is suitable.
				      (append (slot-value i1 'chromosomes)
					      (slot-value i2 'chromosomes))))

			(set-indv-fitness-fn #'(lambda (f1 f2) (+ f1 f2)))
			(max (length population))
			(c 0)
			(new-population nil)
			(selected-parents nil))
  (cond ((= max c)
	 ;; sorted new-population and selected-parents.
	 ;; and result of combine-fn is sorted list.
	 (funcall p-combi-fn new-population selected-parents))
	(t
	 (let ((p1 (funcall select-fn (random 1.0) population)) ;:thinkit:
	       (p2 (funcall select-fn (random 1.0) population)))
	   (let ((m-indvs1 (meiosis (list p1) meiosis-1-fn meiosis-2-fn))
		 (m-indvs2 (meiosis (list p2) meiosis-1-fn meiosis-2-fn)))
	     ;; s1, s2 is each indv
	     (let ((s1 (funcall select-m-indvs-fn m-indvs1))
		   (s2 (funcall select-m-indvs-fn m-indvs2)))
	       (let ((f1 (funcall fitness-fn s1))
		     (f2 (funcall fitness-fn s2)))
		 (generate-next population
				:p-combi-fn p-combi-fn
				:select-fn select-fn
				:meiosis-1-fn meiosis-1-fn
				:meiosis-2-fn meiosis-2-fn
				:select-m-indvs-fn select-m-indvs-fn
				:combi-fn combi-fn
				:fitness-fn fitness-fn
				:set-indv-fitness-fn set-indv-fitness-fn

				:max max
				:c (1+ c)
				:new-population
				(insert-as-sorted-decr (make-indv :chromosomes ;:thinkit:
								  (funcall combi-fn s1 s2)
								  :fitness
								  (funcall set-indv-fitness-fn f1 f2))
						       new-population)
				:selected-parents
				(insert-as-sorted-decr p2 (insert-as-sorted-decr p1 selected-parents))))))))))


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



(defun combi (select-fn get-chromosomes-fn)
  #'(lambda (indvs1 indvs2)
      (let ((selected1 (funcall select-fn indvs1))
	    (selected2 (funcall select-fn indvs2)))
	(list (funcall get-chromosomes-fn selected1) (funcall get-chromosomes-fn selected2)))))

(defstruct chromatid
  nucleic-acids
  fitness)

(defun select-m-indvs (get-indv-fitness-fn)
  (select :key get-indv-fitness-fn))		;:key: todo: (slot-value ...)


(defun set-indv-fitness ()			; f1 f2 f3 ....
  #'(lambda (&optional (acc nil) &rest args)
      (cond ((null args) acc)
	    (t
	     (cond ((null acc) (apply (set-indv-fitness) args))
		   (t
		    (cond ((< acc (car args)) (apply (set-indv-fitness) args))
			  (t
			   (apply (set-indv-fitness) acc (cdr args))))))))))
