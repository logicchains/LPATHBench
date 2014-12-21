
(declaim (optimize (speed 3) (space 0) (debug 0) (safety 0)))

(defstruct route
  (dest 0 :type fixnum)
  (cost 0 :type fixnum))

(defstruct node
  (neighbours (make-array 0 :fill-pointer 0 :adjustable t) :type (vector route)))

(defun split (chars str &optional (lst nil) (accm ""))
  (declare (type simple-string str))
  (cond
    ((zerop (length str))
     (reverse (cons accm lst)))
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
                 for split-line = (split '(#\Space)
                                         (string-right-trim " " line))
                 do (map-into split-line #'parse-integer split-line)
                 collect split-line)
              num-lines))))

(defun parse-places ()
  (multiple-value-bind (place-data num-nodes)
      (read-places)
    (flet ((init-graph (n)
             (let ((graph (make-array n)))
               (map-into graph (lambda (n)
                                 (declare (ignore n))
                                 (make-node))
                         graph)))
           (insert-node (node-list graph)
             (destructuring-bind (node-id neighbour dist)
                 node-list
               (vector-push-extend (make-route :dest neighbour :cost dist)
                                   (node-neighbours (aref graph node-id))))))
      (let ((graph (init-graph num-nodes)))
        (loop for node in place-data do (insert-node node graph))
        graph))))

(defun get-longest-path (nodes &key (start 0))
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 0)
                     (compilation-speed 0)
                     #+lispworks (fixnum-safety 0))
           (type (simple-array node) nodes))
  (let ((visited (make-array (length nodes) :initial-element nil)))
    (declare (type (simple-array atom) visited))
    (flet ((visited-p (n)
             (svref visited n)))
      (labels ((longest-path (node-id)
                 (declare (type fixnum node-id))
                 (setf (svref visited node-id) t)
                 (let ((current-node (svref nodes node-id)))
                   (loop
                      for neighbour of-type route across (node-neighbours current-node)
                      for dest of-type fixnum = (route-dest neighbour)
                      unless (visited-p dest)
                      maximize (the fixnum
                                    (+ (the fixnum (route-cost neighbour))
                                       (the fixnum (longest-path dest))))
                      into max-cost
                      finally (progn
                                (setf (svref visited node-id) nil)
                                (return max-cost))))))
        (longest-path start)))))

(defun run ()
  (let* ((start (get-internal-real-time))
         (len (get-longest-path (parse-places)))
         (duration (- (get-internal-real-time) start)))
    (format t "~d LANGUAGE Lisp ~d ~%" len duration)))

(sb-ext:save-lisp-and-die "lisp" :toplevel #'run :executable t)
