(defmacro aif (if then &optional else)
  `(let ((it ,if))
     (if it
	 ,then
	 ,else)))

(defun maphash-to-list (function hash-table)
  (let ((retvals nil))
    (maphash #'(lambda (key value)
		 (setf retvals (cons (funcall function key value)
				     retvals)))
	     hash-table)
    retvals))

(defun union-all-non-nil (lists)
  (remove nil (reduce #'union lists
		      :initial-value nil)))

(defun set-eq (set1 set2)
  (not (set-exclusive-or set1 set2 :test #'eq)))
