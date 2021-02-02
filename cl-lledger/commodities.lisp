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

(defclass commodity ()
  ((name
    :type     string
    :initarg  :name
    :accessor commodity-name)
   (note
    :type     string
    :initarg  :note
    :accessor commodity-note)
   (format
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
  (:documentation "All quantities are specified in terms of a commodity. It may have a descriptive NOTE attached and must have a unique NAME. If FORMAT is :AMERICAN, then the commodity will be printed with periods as decimal separators and commas as thousands-separators such as '$1,000.00'. If :EUROPEAN, it will be in the format more common in mainland Europe '$1.000,00'. PREPOSITIVE-P specifies whether the commodity name comes before or after the quantity it is to represent. Finally, TICKER optionally represents a ticker which has value information for external functions.")
  (:default-initargs :note          ""
		     :ticker        ""
		     :format        :american
		     :prepositive-p t))

(defclass item-class ()
  ((name
    :type     string
    :initarg  :name
    :accessor item-class-name)
   (attributes
    :type     list
    :initarg  :attributes
    :accessor item-class-attributes)
   (valuation-fn
    :type     (or null function)
    :initarg  :valuation-fn
    :accessor item-class-valuation-fn)
   (balancing-account
    :type     (or null account)
    :initarg  :balancing-account
    :accessor item-class-valuation-account))
  (:documentation "Represents a common set of items which share attributes ATTRIBUTES. It optionally has a VALUATION-FN which determines the market value of its member items (if it is not overriden individually) and a BALANCING-ACCOUNT which balances out market valuation fluctuations (if it is not overriden). If these optional slots are not provided, they are ascertained as necessary according to the principle of subsidiarity. Names must be unique.")
  (:default-initargs :valuation-fn nil :balancing-account nil))

(defclass item ()
  ((name
    :type     string
    :initarg  :name
    :accessor item-name)
   (class
    :type     (or null item-class)
    :initarg  :class
    :accessor item-type)
   (attributes
    :type     list
    :initarg  :attributes
    :accessor item-attributes)
   (cost
    :type     amount
    :initarg  :cost
    :accessor item-cost)
   (valuation-fn
    :type     (or null function)
    :initarg  :valuation-fn
    :accessor item-valuation-fn)
   (balancing-account
    :type     (or null account)
    :initarg  :balancing-account
    :accessor item-balancing-account))
  (:documentation "Individual items are of class ITEM. They optionally have an item-class CLASS and a list of attributes ATTRIBUTES. COST represents the cost basis of the account where the item currently resides. It optionally has a VALUATION-FN to represent the current market value. (If it is not supplied, it is found according to the principle of subsidiarity.) It optionally has a BALANCING-ACCOUNT to post fluctuations of its market value in. Again, if not supplied, it is found according to the principle of subsidiarity. Item names must be unique, if they are supplied. Item cost is usually specified; however, if it is not (if only the cost of the lot is specified and the cost of the item cannot be inferred), it is an error for that item to be posted outside of its lot.")
  (:default-initargs :name "" :class nil :attributes nil :valuation-fn nil :balancing-account nil))

(defclass lot ()
  ((date-purchased
    :type     date
    :initarg  :date
    :accessor lot-date-purchased)
   (name
    :type     string
    :initarg  :name
    :accessor lot-name)
   (cost
    :type     amount
    :initarg  :cost
    :accessor lot-cost))
  (:documentation "Lots represent collections of either items or a single quantity with a cost basis.")
  (:default-initargs :name ""))

(defclass item-lot ()
  ((items
    :type     list
    :initarg  :items
    :accessor item-lot-items))
  (:documentation "ITEM-LOTs hold items."))

(defclass amount-lot ()
  ((amount
    :type     amount
    :initarg  :amount
    :accessor amount-lot-amount))
  (:documentation "Holds individual amount lots (such as '40 MSFT @ $10')."))
