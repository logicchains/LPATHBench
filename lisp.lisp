
(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))

(defstruct route (dest 0 :type fixnum) (cost 0 :type fixnum))

(defstruct node (neighbours (make-array 10 :fill-pointer 0 :adjustable t) :type (vector route)))

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
    (let ((nodes (make-array num-nodes :initial-element (make-node))))
      (labels ((my-loop (i)
			(let ((nums (split '(#\space) (elt place-data i))))
			  (when (> (length place-data) (+ i 1))
			    (let ((node-id (parse-integer (elt nums 0)))
				  (neighbour (parse-integer (elt nums 1)))
				  (dist (parse-integer (elt nums 2))))
			      (vector-push (make-route :dest neighbour :cost dist) (node-neighbours (elt nodes node-id))))
			    (my-loop (+ i 1))))))
	(my-loop 0)
	nodes))))
