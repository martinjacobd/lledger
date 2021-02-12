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

(in-package #:cl-lledger)

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
