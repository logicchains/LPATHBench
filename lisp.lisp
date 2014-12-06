
(declaim (optimize (speed 3) (space 0) (debug 0) (safety 0)))

(defstruct route (dest 0 :type fixnum) (cost 0 :type fixnum))

(defstruct node (neighbours (make-array 0 :fill-pointer 0 :adjustable t) :type (vector route)))

(defun split (chars str &optional (lst nil) (accm ""))
  (cond
   ((= (length str) 0) (reverse (cons accm lst)))
    (t
     (let ((c (char str 0)))
       (if (member c chars)
	   (split chars (subseq str 1) (cons accm lst) "")
	 (split chars (subseq str 1) 
		lst 
		(concatenate 'string
			     accm
			     (string c))))))))

(defun read-places ()
  (with-open-file (stream "agraph")
		  (let ((num-lines (parse-integer (read-line stream nil))))
		    (values (loop
			     for line = (read-line stream nil 'eof)
			     until (eq line 'eof)
			     collect line)
			    num-lines))))

(defun parse-places ()
  (multiple-value-bind (place-data num-nodes) (read-places) 
    (let ((nodes (make-array num-nodes :element-type 'node)))
      (dotimes (i num-nodes)
	(setf (elt nodes i) (make-node)))
      (labels ((my-loop (i)
			(let ((nums (split '(#\space) (elt place-data i))))
			  (when (> (length place-data) (+ i 1))
			    (let ((node-id (parse-integer (elt nums 0)))
				  (neighbour (parse-integer (elt nums 1)))
				  (dist (parse-integer (elt nums 2))))
			      (vector-push-extend (make-route :dest neighbour :cost dist) (node-neighbours (elt nodes node-id))))
			    (my-loop (+ i 1))))))
	(my-loop 0)
	nodes))))


(defun get-longest-path (nodes node-id visited)
  (declare (type fixnum node-id)
	   (type (vector node) nodes)
	   (type (vector atom) visited))
  (setf (elt visited node-id) t)
  (let ((max 0))
    (map nil #'(lambda (neighbour)
		 (declare (type route neighbour))
		 (when (not (elt visited (route-dest neighbour)))
		     (let ((dist (+ (route-cost neighbour) (get-longest-path nodes (route-dest neighbour) visited))))
		       (when (> dist max)
			   (setf max dist))))) 
       (node-neighbours (elt nodes node-id)))
    (setf (elt visited node-id) nil)
    (the fixnum max)))

(defun run ()
  (defparameter nodes (parse-places))
  (defparameter visited (make-array (length nodes) :initial-element nil))
  (defparameter start (get-internal-real-time))
  (defparameter len (get-longest-path nodes 0 visited))
  (defparameter duration (- (get-internal-real-time) start))
  (format t "~d LANGUAGE Lisp ~d ~%" len duration))

(sb-ext:save-lisp-and-die "lisp" :toplevel #'run :executable t)
