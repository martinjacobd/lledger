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

(defclass adjacency-matrix (graph-data)
  ((matrix
    :type     array
    :initarg  :matrix
    :accessor matrix)))

(defclass incidence-matrix (graph-data)
  ((matrix
    :type     array
    :initarg  :matrix
    :accessor matrix)
   (n-edges
    :type     fixnum
    :initarg  :n-edges
    :accessor n-edges)))

(defclass adjacency-list (graph-data)
  ((vertices
    :type     list
    :initform nil
    :initarg  :vertices
    :accessor vertices)))

(defclass vertex ()
  ((value
    :type     t)
   (outgoing-edges
    :type     list
    :initform nil
    :accessor outgoing-edges)
   (index
    :type     fixnum
    :accessor index)))

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
   (vertex-values
    :type     (or null array)
    :initform nil)
   (n-vertices
    :type     (integer 0 *)
    :accessor n-vertices
    :initform 0)
   (size
    :type     (integer 0 *)
    :initarg  :size)
   (resize-size
    :type     (rational 1 *)
    :initarg  :resize-size))
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
					:adjustable t)))
    (:incidence-matrix
     (make-instance 'incidence-matrix
		    :matrix (make-array (list size size)
					:element-type (if weighted-p
							  'integer
							  'bit)
					:initial-element 0
					:adjustable t)
		    :n-edges 0))
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

(defmethod adjacent-p :around ((graph graph) index-a index-b)
  (and (< index-a (n-vertices graph))
       (< index-b (n-vertices graph))
       (call-next-method)))

(defmethod adjacent-p ((graph graph) index-a index-b)
  (assert (typep (graph-data graph) 'adjacency-list))
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
  (assert (typep (graph-data graph) 'adjacency-list))
  (let ((vertex (find-vertes graph index)))
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
	  :accumulate (or (loop :for j :below index
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
	  :accumulate (or (loop :for j :below index
			     :if (= (aref (matrix data) j i) 1)
			     :return j)
			  (loop :for j :from (+ index 1) :to (n-vertices graph)
			     :if (= (aref (matrix data) j i) 1)
			     :return j)
			  index)))
      (adjacency-list
       (call-next-method)))))

