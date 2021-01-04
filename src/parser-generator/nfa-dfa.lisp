(load "../utilities.lisp")

(defclass state ()
  ((char-table
    :type hash-table
    :initform (make-hash-table :test #'eq)
    :reader char-table)
   (predicate-table
     :type list
     :initform nil
     :reader predicate-table)))

(defclass nfa-state (state)
  ())

(defmethod add-transition ((start-state nfa-state)
			   character
			   (end-state nfa-state))
  (with-slots ((current-char-table char-table))
      start-state
    (let ((current-transitions
	   (gethash character current-char-table)))
      (setf (gethash character current-char-table)
	  (adjoin end-state
		  current-transitions)))))

(defmethod add-transition ((start-state nfa-state)
			   (predicate function)
			   (end-state nfa-state))
  (with-slots ((current-predicate-table
		predicate-table))
      start-state
    (let ((current-transitions
	   (assoc predicate current-predicate-table
		  :test #'eq)))
      (if current-transitions
	  (rplacd (assoc predicate
			 current-predicate-table
			 :test #'eq)
		  (adjoin end-state
			  current-transitions))
	  (setf current-predicate-table
		(acons predicate (list end-state)
		       current-predicate-table))))))

(defmethod predicate-transitions ((state nfa-state)
				  character)
  (flet ((ret-preds (predicate-specifier)
	   (if (funcall (first predicate-specifier)
			character)
	       (rest predicate-specifier)
	       nil)))
    (reduce #'append
	    (mapcar #'ret-preds
		    (slot-value state
				'predicate-table))
	    :initial-value nil)))

(defmethod character-transitions ((state nfa-state)
				  character)
  (gethash character (slot-value state 'char-table)))

(defmethod transition-states ((start-state nfa-state)
			      character)
  (append (character-transitions start-state
				 character)
	  (predicate-transitions start-state
				 character)))

(defclass nfa ()
  ((start-state
    :type nfa-state
    :initarg :start-state
    :initform (make-instance 'nfa-state)
    :accessor start-state)
   (end-states
    :type list
    :initarg :end-states
    :initform nil
    :accessor end-states)))

(defmethod end-state-p ((nfa nfa) (state state))
  (member state (end-states nfa)))

(defmethod add-end-state ((nfa nfa) (end-state state))
  (setf (slot-value nfa 'end-states)
	(cons end-state (slot-value nfa 'end-states))))

(defun make-nfa-matches-empty ()
  (let ((nfa       (make-instance 'nfa))
	(end-state (make-instance 'nfa-state)))
    (add-transition (start-state nfa) nil end-state)
    (add-end-state nfa end-state)
    nfa))

(defun make-nfa-matches-char (char)
  (let ((nfa       (make-instance 'nfa))
	(end-state (make-instance 'nfa-state)))
    (add-transition (start-state nfa) char end-state)
    (add-end-state nfa end-state)
    nfa))

(defmethod make-nfa-matches-or ((nfa-a nfa) (nfa-b nfa))
  (let ((nfa       (make-instance 'nfa))
	(end-state (make-instance 'nfa-state)))
    (add-transition (start-state nfa) nil (start-state nfa-a))
    (add-transition (start-state nfa) nil (start-state nfa-b))
    (mapcar #'(lambda (state)
		(add-transition state nil end-state))
	    (slot-value nfa-a 'end-states))
    (mapcar #'(lambda (state)
		(add-transition state nil end-state))
	    (slot-value nfa-b 'end-states))
    (add-end-state nfa end-state)
    nfa))

(defmethod make-nfa-matches-cat ((nfa-a nfa) (nfa-b nfa))
  (let ((nfa (make-instance 'nfa
			    :start-state
			    (start-state nfa-a))))
    (mapcar #'(lambda (state)
		(add-end-state nfa state))
	    (slot-value nfa-b 'end-states))
    (mapcar #'(lambda (state)
		(add-transition state
				nil
				(start-state nfa-b)))
	    (slot-value nfa-a 'end-states))
    nfa))

(defmethod make-nfa-matches-star ((nfa-a nfa))
  (let ((nfa       (make-instance 'nfa))
	(end-state (make-instance 'nfa-state)))
    (add-transition (start-state nfa) 
		    nil
		    end-state)
    (add-transition (start-state nfa)
		    nil
		    (start-state nfa-a))
    (mapc #'(lambda (old-end-state)
	      (add-transition old-end-state
			      nil
			      end-state))
	  (end-states nfa-a))
    (add-transition end-state
		    nil
		    (start-state nfa-a))
    (add-end-state  nfa end-state)
    nfa))

(defclass dfa-state (nfa-state)
  ((label
    :type list
    :initform nil
    :initarg :label
    :reader state-label)))

(defclass dfa (nfa)
  ((states
    :type list
    :initform nil
    :reader dfa-states)
   (start-state
    :type (or dfa-state null)
    :initform nil
    :initarg  :start-state)))

(defmethod add-state ((dfa dfa) (state dfa-state))
  (setf (slot-value dfa 'states)
	(adjoin state
		(dfa-states dfa)
		:key #'state-label
		:test #'set-eq)))

(defmethod dfa-state-with-label ((dfa dfa)
				 label)
  (aif (find label (dfa-states dfa)
	     :key  #'state-label
	     :test #'set-eq)
       it
       (let ((state (make-instance 'dfa-state :label label)))
	 (add-state dfa state)
	 state)))

(let ((memoization (make-hash-table)))
  (defmethod closure ((state nfa-state))
    (aif (gethash state memoization)
	 it
	 (let ((result (aif (transition-states state nil)
			    (union-all-non-nil
			     (mapcar #'closure (remove
						state
						it)))
			    nil)))
	   (setf result
		 (cons state result))
	   (setf (gethash state memoization) result)
	   result)))
  
  (defun clear-closure-memoization ()
    (clrhash memoization)))

(defmethod label-has-state ((dfa dfa) label)
  (member label (dfa-states dfa)
	  :key  #'state-label
	  :test #'set-eq))

(defmethod reachable-states ((state nfa-state))
  (union-all-non-nil
   (list
    (union-all-non-nil (maphash-to-list #'(lambda (key value)
					    (declare (ignore key))
					    value)
					(char-table state)))
    (union-all-non-nil (mapcar #'cdr (predicate-table state))))))

(defun transition-states-with-epsilons (closure
					character)
  (union-all-non-nil
   (mapcar #'closure
	   (union-all-non-nil
	    (remove nil
		    (mapcar #'(lambda (state)
				(transition-states state
						   character))
			    closure))))))

(defmethod chars-in-char-table ((state nfa-state))
  (remove nil (maphash-to-list #'(lambda (key value)
				   (declare (ignore value))
				   key)
			       (char-table state))))

(defun char-table-with-epsilons (closure)
  (let ((char-table (make-hash-table :test #'eq))
	(all-chars  (union-all-non-nil
		     (mapcar #'chars-in-char-table
			     closure))))
    (mapc #'(lambda (char)
	      (setf (gethash char char-table)
		    (transition-states-with-epsilons closure
						     char)))
	  all-chars)
    char-table))

(defmethod funs-in-predicate-table ((state nfa-state))
  (mapcar #'car (predicate-table state)))

(defun fun-maps-with-epsilons (closure
			       predicate)
  (union-all-non-nil
   (mapcar #'closure
	   (union-all-non-nil
	    (mapcar #'(lambda (state)
			(assoc predicate
			       (predicate-table
				state)
			       :test #'eq))
		    closure)))))

(defun pred-table-with-epsilons (closure)
  (mapcar #'(lambda (function)
	      (cons function (fun-maps-with-epsilons closure
						     function)))
	  (union-all-non-nil
		  (mapcar #'funs-in-predicate-table
			  closure))))

(let ((states-already-converted nil))
  (defmethod dfa-state-from-closure ((dfa dfa)
				     closure)
    (if (member closure states-already-converted
		:test #'set-eq)
	nil
	(let ((new-state      (dfa-state-with-label dfa closure))
	      (new-char-table (char-table-with-epsilons closure))
	      (new-pred-table (pred-table-with-epsilons closure))
	      (states-to-convert nil))
	  (push closure states-already-converted)
	  (mapc #'(lambda (closure)
		    (setf states-to-convert
			  (adjoin closure
				  states-to-convert)))
		(maphash-to-list #'(lambda (key value)
				     (declare (ignore key))
				     value)
				 new-char-table))
	  (mapc #'(lambda (closure)
		    (setf states-to-convert
			  (adjoin closure
				  states-to-convert)))
		(mapcar #'cdr new-pred-table))
	  (mapc #'(lambda (closure)
		    (dfa-state-from-closure dfa closure))
		(set-difference states-to-convert
				states-already-converted))
	  (maphash #'(lambda (key value)
		       (setf (gethash key new-char-table)
			     (dfa-state-with-label dfa value)))
		   new-char-table)
	  (mapc #'(lambda (predicate-specifier)
		    (rplacd predicate-specifier
			    (list (dfa-state-with-label
				   dfa
				   (cdr predicate-specifier)))))
		new-pred-table)
	  (setf (slot-value new-state 'char-table)
		new-char-table)
	  (setf (slot-value new-state 'predicate-table)
		new-pred-table)
	  new-state))))
    
(defmethod nfa-to-dfa ((nfa nfa))
  (clear-closure-memoization)
  (let* ((dfa (make-instance 'dfa))
	 (start-state (dfa-state-from-closure
		       dfa
		       (closure (start-state nfa)))))
    (setf (slot-value dfa 'start-state) start-state)
    (mapc #'(lambda (state)
	      (if (intersection (state-label state)
				(end-states nfa))
		  (add-end-state dfa state)))
	  (dfa-states dfa))
    dfa))

(defmethod transition-state ((state dfa-state)
			     character)
  (aif (gethash character (char-table state))
       it
       (loop :for predicate-specifier :in (predicate-table state)
	  :do
	    (aif (funcall (car predicate-specifier) character)
		 (return-from transition-state it)))))

(defmethod traverse-states ((state dfa-state) (string string))
  (traverse-states state (map 'list #'values string)))

(defmethod traverse-states ((state dfa-state) (string list))
  (if (null string)
      state
      (aif (transition-state state (first string))
	   (traverse-states it (rest string))
	   nil)))

(defmethod dfa-matches-string-p ((dfa dfa) (string string))
  (aif (traverse-states (start-state dfa)
			string)
       (end-state-p dfa it)
       nil))


