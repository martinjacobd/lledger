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
    :initarg :weight)
   (weight-assigned-p
    :type    boolean
    :initarg weight-assigned-p)))

(defclass graph ()
  ((graph-data
    :type     graph-data
    :accessor graph-data
    :initarg  :data)
   (vertices
    :type     list
    :accessor vertices
    :initarg  :vertices
    :initform nil)
   (n-vertices
    :type     (integer 0 *)
    :accessor n-vertices
    :initform 0))
  (:documentation
   "Generic graph class. Graphs may be weighted or unweighted (on the edges) and directed or undirected. All graphs may have values associated with the vertices, but only weighted graphs may have values associated with the edges."))

(defgeneric adjacent-p (graph index-a index-b)
  (:documentation "Tests whether there is an edge from the vertex indexed by INDEX-A to that indexed by INDEX-B in GRAPH"))

(defgeneric neighbors (graph index)
  (:documentation "Lists indices of all vertices to which the vertex indexed by INDEX has an edge in GRAPH."))

(defgeneric add-vertex (graph &optional value)
  (:documentation "Expands GRAPH to include one more vertex whose index is returned. If VALUE is specified, the vertex has associated with it the value VALUE."))

(defgeneric add-edge (graph index-a index-b &optional weight)
  (:documentation "Adds an edge in GRAPH from the vertex indexed by INDEX-A to that indexed by INDEX-B. It is an error for WEIGHT not to be specified for a weighted graph, and for it to be specified in an unweighted graph."))

(defgeneric remove-vertex (graph index)
  (:documentation "Removes the vertex indexed by INDEX from graph GRAPH. Note: This may cause the indices of the vertices to shift."))

(defgeneric remove-edge (graph index-a index-b &optional weight)
  (:documentation "Removes the edge from the vertex indexed by INDEX-A to the vertex indexed by INDEX-B. Note: It is possible that there is more than one edge from INDEX-A to INDEX-B. In the case that there is more than one edge, it is an error for WEIGHT not to be specified, and all edges with weight EQL to that specified will be removed."))

(defgeneric vertex-value (graph index)
  (:documentation "Returns the value associated with the vertex indexed by INDEX in GRAPH."))

(defgeneric edge-value (graph index-a index-b)
  (:documentation "Returns the value associated with the edge from the vertex indexed by INDEX-A to that indexed by INDEX-B in the weighted graph GRAPH."))
