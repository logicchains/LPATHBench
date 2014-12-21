; https://github.com/logicchains/LPATHBench/blob/master/writeup.md

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defstruct route
    (dest 0 :type fixnum)
    (cost 0 :type fixnum)))

(defun parse-line (line &aux (pos 0) n)
  (declare (ignorable n))
  (loop repeat 3
        collect (multiple-value-setq (n pos)
                    (parse-integer line :start pos :junk-allowed t))))

(defparameter *file* "agraph")

(defun read-places ()
  (with-open-file (stream *file*)
    (let ((num-lines (parse-integer (read-line stream nil))))
      (values (loop for line = (read-line stream nil nil)
                    while line
                    collect (parse-line line))
              num-lines))))

(defun parse-places ()
  (multiple-value-bind (place-data num-nodes)
      (read-places) 
    (let ((nodes (make-array num-nodes :initial-element nil)))
      (loop for (node-id neighbour dist) in place-data
            do (push (make-route :dest neighbour :cost dist)
                     (aref nodes node-id)))
      nodes)))

(declaim (ftype (function (simple-vector fixnum simple-vector) fixnum)
                get-longest-path))

(defun get-longest-path (nodes node-id visited &aux (max 0))
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 0) (compilation-speed 0)
           #+lispworks (fixnum-safety 0))
           (fixnum max))
  (setf (svref visited node-id) t)
  (setf max (loop for neighbour of-type route in (svref nodes node-id)
                  unless (svref visited (route-dest neighbour))
                  maximize (+ (the fixnum (route-cost neighbour))
                              (the fixnum (get-longest-path nodes
                                                            (route-dest neighbour)
                                                            visited)))
                  #+lispworks fixnum))
  (setf (svref visited node-id) nil)
  max)
 
(defun run ()
  (let* ((nodes    (parse-places))
         (visited  (make-array (length nodes) :element-type 'boolean :initial-element nil))
         (start    (get-internal-real-time))
         (len      (get-longest-path nodes 0 visited))
         (end      (get-internal-real-time))
         (duration (truncate (* 1000 (- end start))
                             internal-time-units-per-second)))
    (format t "~d LANGUAGE Lisp ~d ~%" len duration)))

(sb-ext:save-lisp-and-die "lisp" :toplevel #'run :executable t)
