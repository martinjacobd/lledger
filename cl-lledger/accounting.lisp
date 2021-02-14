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

(defclass date ()
  ((year
    :type integer
    :initarg :)))

(defmacro choose-specific-alternative (&rest alternatives)
  "From among a list of alternatives, ordered from the most specific to the least specific, returns the most specific non-NIL argument or an error if none are specified."
  (or ,@alternatives
      (error "Cannot find from among alternatives, defaults not specified.")))
