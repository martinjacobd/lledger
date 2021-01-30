;;; Copyright (c) Jacob Martin 2021

;;; This file is part of lledger.

;;; lledger is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; lledger is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with lledger.  If not, see <https://www.gnu.org/licenses/>.

(defpackage graph
  (:use    "COMMON-LISP")
  (:export "WEIGHTED-DIR-GRAPH"
	   "UNWEIGHTED-DIR-GRAPH"
	   "WEIGHTED-UND-GRAPH"
	   "UNWEIGHTED-UND-GRAPH"
	   "ADJACENT-P"
	   "NEIGHBORS"
	   "ADD-VERTEX"
	   "ADD-EDGE"
	   "REMOVE-VERTEX"
	   "REMOVE-EDGE"
	   "VERTEX-VALUE"
	   "EDGE-VALUE"))

(in-package graph)

(defclass graph-data ()
  ())

(defclass matrix-data (graph-data)
  ((matrix
    :type     array
    :initarg  :matrix
    :accessor matrix)
   (vertex-values
    :type     (vector fixnum)
    :initarg  :vertex-values
    :accessor vertex-values)
   (vertex-values-assigned-p
    :type    (vector bit)
    :initarg  :vertex-values-assigned-p
    :accessor vertex-values-assigned-p)))

(defclass adjacency-matrix (matrix-data)
  ())

(defclass incidence-matrix (matrix-data)
  ((n-edges
    :type     fixnum
    :initarg  :n-edges
    :accessor n-edges)
   (size-edges
    :type     fixnum
    :initarg  :size-edges
    :accessor size-edges)))

(defclass adjacency-list (graph-data)
  ((vertices
    :type     list
    :initform nil
    :initarg  :vertices
    :accessor vertices)))

(defclass vertex ()
  ((value
    :type     t
    :initform nil
    :initarg  :value)
   (value-assigned-p
    :type     boolean
    :initarg  :value-assigned-p)
   (outgoing-edges
    :type     list
    :initform nil
    :accessor outgoing-edges)
   (index
    :type     fixnum
    :accessor index
    :initarg  :index)))

(defclass edge ()
  ((end-vertex
    :type     vertex
    :initarg  :end-vertex
    :accessor end-vertex)
   (weight
    :type t
    :initarg :weight)))

(defclass graph ()
  ((graph-data
    :type     graph-data
    :accessor graph-data
    :initarg  :data)
   (n-vertices
    :type     (integer 0 *)
    :accessor n-vertices
    :initform 0)
   (size
    :type     (integer 0 *)
    :initarg  :size
    :accessor size)
   (resize-size
    :type     (rational 1 *)
    :initarg  :resize-size
    :accessor resize-size))
  (:documentation
   "Generic graph class. Graphs may be weighted or unweighted (on the edges) and directed or undirected. All graphs may have values associated with the vertices, but only weighted graphs may have values associated with the edges. Graphs may be represented by an adjacency matrix, an incidence matrix, or an adjacency list. The :SIZE and :RESIZE-SIZE arguments work like the :SIZE and :REHASH-SIZE arguments to MAKE-HASH-TABLE if the graph is represented by a matrix. They are ignored if the graph is represented by an adjacency list. If a weighted graph is represented by a matrix, it may only have integer values associated with the edges.")
  (:default-initargs :size 10 :resize-size 2))

(defgeneric adjacent-p (graph index-a index-b)
  (:documentation "Tests whether there is an edge from the vertex indexed by INDEX-A to that indexed by INDEX-B in GRAPH"))

(defgeneric neighbors (graph index)
  (:documentation "Lists indices of all vertices to which the vertex indexed by INDEX has an edge in GRAPH."))

(defgeneric add-vertex (graph &optional value)
  (:documentation "Expands GRAPH to include one more vertex whose index is returned. If VALUE is specified, the vertex has associated with it the value VALUE."))

(defgeneric add-edge (graph index-a index-b &optional weight)
  (:documentation "Adds an edge in GRAPH from the vertex indexed by INDEX-A to that indexed by INDEX-B. It is an error for WEIGHT not to be specified for a weighted graph, and for it to be specified in an unweighted graph. WEIGHT must be an integer if GRAPH is represented by an adjacency matrix or incidence matrix; if GRAPH is a weighted graph represented by an adjacency list, it may be any value including NIL."))

(defgeneric remove-vertex (graph index)
  (:documentation "Removes the vertex indexed by INDEX from graph GRAPH. Note: This may cause the indices of the vertices to shift."))

(defgeneric remove-edge (graph index-a index-b &optional weight)
  (:documentation "Removes the edge from the vertex indexed by INDEX-A to the vertex indexed by INDEX-B. Note: In an adjacency list, it is possible that there is more than one edge from INDEX-A to INDEX-B. In that case, it is an error for WEIGHT not to be specified, and all edges with weight EQL to that specified will be removed. WEIGHT is not checked if more than edge is not found."))

(defgeneric vertex-value (graph index)
  (:documentation "Returns the value associated with the vertex indexed by INDEX in GRAPH."))

(defclass undirected-graph (graph)
  ())

(defclass directed-graph (graph)
  ())

(defclass weighted-graph (graph)
  ())

(defclass unweighted-graph (graph)
  ())

(defgeneric edge-value (graph index-a index-b)
  (:documentation "Returns the value associated with the edge from the vertex indexed by INDEX-A to that indexed by INDEX-B in the weighted graph GRAPH. It is zero if there is no such edge."))

;;; The following are the only classes that should be used by code outside of this file.

(defclass weighted-dir-graph (weighted-graph directed-graph)
  ())

(defclass unweighted-dir-graph (unweighted-graph directed-graph)
  ())

(defclass weighted-und-graph (weighted-graph undirected-graph)
  ())

(defclass unweighted-und-graph (unweighted-graph undirected-graph)
  ())

(defun make-empty-graph-data (&key
			  (size 10)
			  (representation :adjacency-matrix)
			  (weighted-p nil))
  (ecase representation
    (:adjacency-matrix
     (make-instance 'adjacency-matrix
		    :matrix (make-array (list size size)
					:element-type (if weighted-p
							  'integer
							  'bit)
					:initial-element 0
					:adjustable t)
		    :vertex-values (make-array size
					       :element-type 'fixnum
					       :initial-element 0
					       :adjustable t)
		    :vertex-values-assigned-p (make-array size
							  :element-type 'bit
							  :initial-element 0
							  :adjustable t)))
    (:incidence-matrix
     (make-instance 'incidence-matrix
		    :matrix (make-array (list size size)
					:element-type (if weighted-p
							  'integer
							  'bit)
					:initial-element 0
					:adjustable t)
		    :n-edges 0
		    :size-edges size
		    :vertex-values (make-array size
					       :element-type 'fixnum
					       :initial-element 0
					       :adjustable t)
		    :vertex-values-assigned-p (make-array size
							  :element-type 'bit
							  :initial-element 0
							  :adjustable t)))
    (:adjacency-list
     (make-instance 'adjacency-list
		    :vertices nil))))

(defun make-empty-graph (&key (size 10)
			   (resize-size 2)
			   (representation :adjacency-matrix)
			   (weighted-p nil)
			   (directed-p nil))
  (let ((empty-data (make-empty-graph-data :size           size
					   :representation representation
					   :weighted-p     weighted-p)))
    (make-instance (cond
		     ((and weighted-p directed-p)
		      'weighted-dir-graph)
		     ((and (not weighted-p) directed-p)
		      'unweighted-dir-graph)
		     ((and weighted-p (not directed-p))
		      'weighted-und-graph)
		     ((and (not weighted-p) (not directed-p))
		      'unweighted-und-graph))
		   :size size
		   :resize-size resize-size
		   :data empty-data)))

(defmethod find-vertex ((graph graph) index)
  (check-type (graph-data graph) adjacency-list)
  (nth (- (length (vertices (graph-data graph))) index 1) (vertices (graph-data graph))))

(defmethod adjacent-p :around ((graph graph) index-a index-b)
  (and (< index-a (n-vertices graph))
       (< index-b (n-vertices graph))
       (call-next-method)))

(defmethod adjacent-p ((graph graph) index-a index-b)
  (check-type (graph-data graph) adjacency-list)
  (dolist (edge (outgoing-edges (find-vertex graph index-a)) nil)
    (when (= (index (end-vertex edge)) index-b)
      (return t))))

(defmethod adjacent-p ((graph undirected-graph) index-a index-b)
  (let ((data (graph-data graph)))
    (etypecase data
      (adjacency-matrix
       (= (bit (matrix data) index-a index-b) 1))
      (incidence-matrix
       (dotimes (i (n-edges (graph-data graph)))
	 (when (and (= (bit (matrix data) index-a i) 1)
		    (= (bit (matrix data) index-b i) 1))
	   (return t))))
      (adjacency-list
       (call-next-method)))))

(defmethod adjacent-p ((graph directed-graph) index-a index-b)
  (let ((data (graph-data graph)))
    (etypecase data
      (adjacency-matrix
       (= (aref (matrix data) index-a index-b) 1))
      (incidence-matrix
       (dotimes (i (n-edges (graph-data graph)))
	 (when (and (= (aref (matrix data) index-a i) -1)
		    (= (aref (matrix data) index-b i) 1))
	   (return t))))
      (adjacency-list
       (call-next-method)))))

(defmethod neighbors :around ((graph graph) index)
  (assert (< index (n-vertices graph)))
  (call-next-method))

(defmethod neighbors ((graph graph) index)
  (check-type (graph-data graph) adjacency-list)
  (let ((vertex (find-vertex graph index)))
    (loop :for edge :in (outgoing-edges vertex)
       :collect (index (end-vertex edge)))))

(defmethod neighbors ((graph undirected-graph) index)
  (let ((data (graph-data graph)))
    (etypecase data
      (adjacency-matrix
       (loop :for i :below (n-vertices graph)
	  :if (= (bit (matrix data) index i) 1)
	  :collect i))
      (incidence-matrix
       (loop :for i :below (n-edges data)
	  :if (= (bit (matrix data) index i) 1)
	  :collect (or (loop :for j :below index
			  :if (= (bit (matrix data) j i) 1)
			  :return j)
		       (loop :for j :from (+ index 1) :to (n-vertices graph)
			  :if (= (bit (matrix data) j i) 1)
			  :return j)
		       index)))
      (adjacency-list
       (call-next-method)))))		       
		   
(defmethod neighbors ((graph directed-graph) index)
  (let ((data (graph-data graph)))
    (etypecase data
      (adjacency-matrix
       (loop :for i :below (n-vertices graph)
	  :if (= (aref (matrix data) index i) 1)
	  :collect i))
      (incidence-matrix
       (loop :for i :below (n-edges data)
	  :if (= (aref (matrix data) index i) -1)
	  :collect (or (loop :for j :below index
			  :if (= (aref (matrix data) j i) 1)
			  :return j)
		       (loop :for j :from (+ index 1) :to (n-vertices graph)
			  :if (= (aref (matrix data) j i) 1)
			  :return j)
		       index)))
      (adjacency-list
       (call-next-method)))))

(defmethod vertex-value :around ((graph graph) index)
  (check-type index fixnum)
  (assert (< index (n-vertices graph)))
  (call-next-method))

(defmethod vertex-value ((graph graph) index)
  (let ((data (graph-data graph)))
    (etypecase (graph-data graph)
      (matrix-data
       (values (aref (vertex-values data) index)
	       (= (bit (vertex-values-assigned-p data) index) 1)))
      (adjacency-list
       (values (slot-value (find-vertex graph index) 'value)
	       (slot-value (find-vertex graph index) 'value-assigned-p))))))

(defmethod (setf vertex-value) :around (vertex-value (graph graph) index)
  (check-type index fixnum)
  (call-next-method))

(defmethod (setf vertex-value) (vertex-value (graph graph) index)
  (let ((data (graph-data graph)))
    (etypecase data
      (matrix-data
       (check-type vertex-value fixnum)
       (setf (bit  (vertex-values-assigned-p data) index) 1)
       (setf (aref (vertex-values data) index) vertex-value))
      (adjacency-list
       (setf (slot-value (find-vertex graph index) 'value) vertex-value)
       (setf (slot-value (find-vertex graph index) 'value-assigned-p) t)))
    vertex-value))

(defmethod add-vertex :around ((graph graph) &optional value)
  (when (not (typep (graph-data graph) 'adjacency-list))
    (check-type value (or integer null)))
  (call-next-method))

(defmethod add-vertex ((graph graph) &optional (value nil val-supplied-p))
  (let ((data (graph-data graph)))
    (etypecase data
      (matrix-data
       (let ((index (n-vertices graph)))
	 (when (<= (size graph) index)
	   (let ((new-size (ceiling (* (size graph) (resize-size graph)))))
	     (adjust-array (matrix data)
			   (list new-size
				 (etypecase data
				   (adjacency-matrix
				    new-size)
				   (incidence-matrix
				    (size-edges data))))
			   :initial-element 0)
	     (adjust-array (vertex-values data)
			   new-size
			   :initial-element 0)
	     (adjust-array (vertex-values-assigned-p data)
			   new-size
			   :initial-element 0)
	     (setf (size graph) new-size)))
	 (when val-supplied-p
	   (setf (vertex-value graph index) value))
	 index))
      (adjacency-list
       (let ((index (if (vertices data)
			(1+ (index (first (vertices data))))
			0)))
	 (push (make-instance 'vertex
			      :index index
			      :value value
			      :value-assigned-p val-supplied-p)
	       (vertices data))
	 index)))))

(defmethod add-vertex :after ((graph graph) &optional value)
  (declare (ignore value))
  (incf (n-vertices graph)))

(defmethod add-edge :around ((graph graph) index-a index-b &optional (weight 1 weight-supplied-p))
  (if weight-supplied-p
      (check-type graph weighted-graph)
      (check-type graph unweighted-graph))
  (assert (and (< index-a (n-vertices graph)) (< index-b (n-vertices graph))))
  (call-next-method))

(defmethod add-edge ((graph directed-graph) index-a index-b &optional (weight 1))
  (declare (ignore weight))
  (let ((data (graph-data graph)))
    (etypecase data
      (adjacency-matrix
       (if (= index-a index-b)
	   (setf (aref (matrix graph) index-a index-b) weight)
	   (progn
	     (setf (aref (matrix graph) index-a index-b) weight)
	     (setf (aref (matrix graph) index-b index-a) (- weight))))
       (call-next-method))
      (incidence-matrix
       (when (<= (size-edges data) (n-edges data))
	 (adjust-array (matrix data) (list (n-vertices graph)
					   (ceiling (* (resize-size graph) (size-edges data)))))
	 (setf (size-edges data) (ceiling (* (resize-size graph) (size-edges data)))))
       (let ((index (n-edges data)))
	 (setf (aref (matrix data) index-a index) (- weight))
	 (setf (aref (matrix data) index-b index) weight))
       (incf (n-edges data))
       (call-next-method))
      (adjacency-list
       (call-next-method)))))

(defmethod add-edge ((graph undirected-graph) index-a index-b &optional weight)
  (declare (ignore weight))
  (let ((data (graph-data graph)))
    (etypecase data
      (adjacency-matrix
       (setf (bit (matrix graph) index-a index-b) 1)
       (setf (bit (matrix graph) index-b index-a) 1)
       (call-next-method))
      (incidence-matrix
       (when (<= (size-edges data) (n-edges data))
	 (adjust-array (matrix data) (list (n-vertices graph)
					   (ceiling (* (resize-size graph) (size-edges data)))))
	 (setf (size-edges data) (ceiling (* (resize-size graph) (size-edges data)))))
       (let ((index (n-edges data)))
	 (setf (bit (matrix data) index-a index) 1)
	 (setf (bit (matrix data) index-b index) 1))
       (incf (n-edges data))
       (call-next-method))
      (adjacency-list
       (call-next-method)))))

(defmethod add-edge ((graph weighted-graph) index-a index-b)
 
