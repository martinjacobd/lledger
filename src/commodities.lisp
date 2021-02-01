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

(in-package #:lledger)

(defclass commodity ()
  ((name
    :type     string
    :initarg  :name
    :accessor commodity-name)
   (note
    :type     string
    :initarg  :note
    :accessor commodity-note))
  (:documentation "All amounts must be given in terms of commodities. They have a NAME, which is used in the input file. If a commodity is declared with a <commodity> directive, then it may have a descriptive note attached also.")
  (:default-initargs :note ""))

(defclass inventory-commodity (commodity)
  ((base-commodity
    :type     (or basic-commodity null)
    :initarg  :base-commodity
    :accessor commodity-base))
  (:documentation "An inventory commodity represents a commodity that may only be made of discrete (whole numbers) of items. Optionally, a BASE-COMMODITY is specified which is the default commodity to which the inventory-commodity is converted on reports and in balances, which must always be specified in a basic-commodity.")
  (:default-initargs :base-commodity nil))

(defclass basic-commodity (commodity)
  ((format
    :type     (member :american :european)
    :initarg  :format
    :accessor commodity-format)
   (prepositive-p
    :type     boolean
    :initarg  :prepositive-p
    :accessor commodity-prepositive-p)
   (ticker
    :type      string
    :initarg   :ticker
    :accessor  commodity-ticker))
  (:documentation "A basic commodity is a commodity in which account balances are allowed to be specified. If FORMAT is :AMERICAN, then the commodity will be printed with periods as decimal separators and commas as thousands-separators such as '$1,000.00'. If :EUROPEAN, it will be in the format more common in mainland Europe '$1.000,00'. PREPOSITIVE-P specifies whether the commodity name comes before or after the quantity it is to represent. Finally, TICKER optionally represents a ticker which has value information for external functions.")
  (:default-initargs :format        :american
	      :prepositive-p t))
