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

(defclass graph-data ())

(defclass adjacency-matrix (graph-data)
  ((matrix
    :type    array
    :initarg matrix)))

(defclass incidence-matrix (graph-data)
  ((matrix
    :type    array
    :initarg matrix)))

(defclass adjacency-list (graph-data)
  ((list
    :type     list
    :initform nil
    :initarg  :list)))

(defclass graph ()
  ((graph-data
    :type     graph-data)
   (vertex-values
    :type     (or null array))
   (n-vertices
    :type     (integer 0 *)
    :initform 0)
   (size
    :type     (integer 0 *)
    :initarg  :size)
   (resize-size
    :type     (rational 1 *)
    :initarg  :resize-size))
  ((:documentation
    "Generic graph class for weighted, directed, or undirected graphs. All graphs may have values associated with the vertices, but only weighted graphs may have (integer) values associated with the edges. Graphs may be represented by an adjacency matrix, an incidence matrix, or an adjacency list. The :SIZE and :RESIZE-SIZE arguments work like the :SIZE and :REHASH-SIZE arguments to MAKE-HASH-TABLE if the graph is represented by a matrix. They are ignored if the graph is represented by an adjacency list.")
   (:default-initargs
       (:size 10)
       (:resize-size 2))))

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

(defgeneric set-vertex-value (graph index value)
  (:documentation "Sets the value of the vertex indexed by INDEX in GRAPH to VALUE."))

(defclass undirected-graph (graph)
  ())

(defclass weighted-graph (graph)
  ())

(defclass directed-graph (graph)
  ())
