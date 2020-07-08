;;;; display.lisp

(in-package #:gnt)

(defvar diamond '(0.5 .5 0.0
		  0.5 -0.5 .0
		  -.5 -0.5 0.0
		  -0.5 0.5 0.0))

(defvar colors '(1.0 0.0 0.0
		 0.0 1.0 0.0
		 0.0 0.0 1.0))

(defvar indices '(0 1 3
		  1 2 3))

(defun setting-before-rendering ()
  (let ((vertex-shader (gl:create-shader :vertex-shader))
	(fragment-shader (gl:create-shader :fragment-shader)))

      (with-open-file (s "vertex.glsl")
	(let ((contents (make-string (file-length s))))
	  (read-sequence contents s)
	  (gl:shader-source vertex-shader contents)
	  (gl:compile-shader vertex-shader)
	  (print (gl:get-shader-info-log
	   vertex-shader))
	  ))
      (with-open-file (s "fragment.glsl")
	(let ((contents (make-string (file-length s))))
	  (read-sequence contents s)
	  (gl:shader-source fragment-shader contents)
	  (gl:compile-shader fragment-shader)
	  (print (gl:get-shader-info-log
		  fragment-shader))))

      (let ((shader-program (gl:create-program)))
	(gl:attach-shader shader-program vertex-shader)
	(gl:attach-shader shader-program fragment-shader)
	(gl:link-program shader-program)
	(gl:delete-shader vertex-shader)
	(gl:delete-shader fragment-shader)
	(print (gl:get-program-info-log
		shader-program))

	(let ((vao (gl:gen-vertex-arrays 1))
	      (vbo (gl:gen-buffers 1))
	      (ebo (gl:gen-buffer)))

	  (gl:bind-vertex-array (car vao))
	  ;; set vbo to GL_ARRAY_BUFFER type
	  (gl:bind-buffer :array-buffer (car vbo))
;;	  ;; store data at memory of graphic card
	  (gl:buffer-data :array-buffer :static-draw
			  (make-gl-array diamond :float (length diamond)))

	  (gl:bind-buffer :element-array-buffer ebo)
	  (gl:buffer-data :element-array-buffer :static-draw
			  (make-gl-array indices :unsigned-int (length indices)))


	  (gl:vertex-attrib-pointer 0 3 :float :false (* 4 3) 0)
	  (gl:enable-vertex-attrib-array 0)

	  (gl:bind-buffer :array-buffer 0)
	  (gl:bind-vertex-array 0)

	  (values vao  shader-program)))))

(defun sdl2-run ()

  (sdl2:with-init (:everything)
    ;; not setting: version 2.1
    ;; setting: version 4.1
    (sdl2:gl-set-attr :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+)
    (sdl2:with-window (win)
      (sdl2:with-gl-context (gl-context win)
	(multiple-value-bind (vao sp) (setting-before-rendering)
	  (sdl2:gl-set-swap-interval 1)


	  (sdl2:with-event-loop (:method :poll)
	    (:idle ()
		   (gl:clear-color 0.0 .5 0.0 1.0)
		   (gl:clear :color-buffer-bit)


		   (gl:use-program sp)

		   (gl:bind-vertex-array (car vao))
		   (%gl:draw-elements :triangles 6 :unsigned-int 0)

		   (sdl2:gl-swap-window win))
	    (:quit () t))))

      )))

(defun display ()
  (bt:interrupt-thread
   (sb-thread:main-thread)
   (lambda ()
     (sdl2:make-this-thread-main
      #'sdl2-run))))


(defun make-gl-array (seq type size &optional
				      (gl-array (gl:alloc-gl-array type size))
				      (c 0))
  (cond ((null seq) gl-array)
	(t
	 (setf (gl:glaref gl-array c) (car seq))
	 (make-gl-array (cdr seq) type size gl-array (1+ c)))))

;; (defclass gl-program ()
;;   ((%program-id :initform -1 :initarg :id :accessor id)
;;    (%v-shader :initform "" :initarg :v-shader :accessor v-shader)
;;    (%f-shader :initform "" :initarg :f-shader :accessor f-shader)))


(defun set-shader-by-file (shader filename)
  (with-open-file (s filename)
    (let ((contents (make-string (file-length s))))
      (read-sequence contents s)
      (gl:shader-source shader contents)
      (gl:compile-shader shader)
      (assert-no-errors shader :shader)
      shader)))

(defun get-shader-program (set-shader-fn)
  #'(lambda (vs-filename fs-filename)
      (let ((vertex-shader (gl:create-shader :vertex-shader))
	    (fragment-shader (gl:create-shader :fragment-shader)))

	(funcall set-shader-fn vertex-shader vs-filename)
	(funcall set-shader-fn fragment-shader fs-filename)

	(let ((shader-program (gl:create-program)))
	  (gl:attach-shader shader-program vertex-shader)
	  (gl:attach-shader shader-program fragment-shader)
	  (gl:link-program shader-program)
	  (gl:delete-shader vertex-shader)
	  (gl:delete-shader fragment-shader)
	  (assert-no-errors shader-program :program)
	  shader-program))))

(defun assert-no-errors (id type)
  (let ((success (cffi:foreign-alloc :int :initial-element 0)))
    (unwind-protect
	 (progn
	   (funcall (read-from-string (concatenate 'string "%gl:get-" (string type) "-iv"))
		    id :compile-status success)

	   (when (/= 1 (cffi:mem-aref success :int))
	     (sr-format "OPENGL ERROR: "
			(funcall
			 (read-from-string
			  (concatenate 'string "gl:get-" (string type) "-info-log"))
			 id))))
      (cffi:foreign-free success))))
