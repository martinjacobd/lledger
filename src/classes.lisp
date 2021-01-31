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

(defclass quantity ()
  ()
  (:documentation "Superclass for all kinds of quantity that may need arithmetic to be performed between them."))

(defclass amount (quantity)
  ((value
    :type     rational
    :initarg  :value
    :accessor amount-value)
   (commodity
    :type     commodity
    :initarg  :commodity
    :accessor amount-commodity))
  (:documentation "An amount is attached to every posting and consists of a rational VALUE and a commodity object COMMODITY."))

(defclass posting-amount (amount)
  ((type
    :type     (member nil :debit :credit)
    :initarg  :type
    :accessor posting-amount-type))
  (:documentation "For the amounts in postings (either from transactions or on an account), which must either be debits or credits. It is inferred from context if not supplied. A :DEBIT or :CREDIT TYPE is specified to determine the column the amount goes in in reports. If neither is specified, it is inferred from the context."))

(defmethod initialize-instance :after ((instance posting-amount) &key value type commodity)
  (declare (ignore commodity))
  ;; By default, just as in `ledger`, non-negative amounts represent
  ;; debits and negative amounts credits.
  (when (null type)
    (if (>= value 0)
	(setf (amount-type instance) :debit)
	(progn
	  (setf (amount-type instance) :credit)
	  (setf (amount-value instance) (- value))))))

(defclass simple-balance ()
  ((amounts
    :type     list
    :initarg  :amounts
    :accessor simple-balance-amounts)
   (type
    :type     (member nil :debit :credit)
    :initarg  :type
    :accessor simple-balance-type))
  (:documentation "Occasionally, a quantity is required to have amounts in multiple commodities, simple balance is used for this purpose."))

(defclass balance ()
  ((debit-balance
    :type     simple-balance
    :accessor debit-balance)
   (credit-balance
    :type     simple-balance
    :accessor debit-balance))
  (:documentation "A balance object will carry both the debits and credits side of a transaction or account."))

(defgeneric plus (quantity-a quantity-b)
  (:documentation "Adds together two quantities. If QUANTITY-A and QUANTITY-B are both of type AMOUNT and have the same COMMODITY, then an AMOUNT of that commodity is returned. If they have differing commodities, then a SIMPLE-BALANCE of TYPE nil is returned with QUANTITY-A and QUANTITY-B copied to the AMOUNTS slot. If QUANTITY-A and QUANTITY-B are both of type POSTING-AMOUNT and have the same COMMODITY, then a POSTING-AMOUNT of that commodity is returned, with TYPE being either :DEBIT or :CREDIT depending on which allows the returned amount to have a positive value (or :DEBIT in the case they cancel). A SIMPLE-BALANCE and AMOUNT or POSTING-AMOUNT add together as you might expect, with quantities of the same commodity condensed appropriately. It is, however, an error for a POSTING-AMOUNT and SIMPLE-BALANCE of different TYPE to be added together. Similarly, a BALANCE and a SIMPLE-BALANCE add as expected, as do a BALANCE and POSTING-AMOUNT. It is an error for a BALANCE to be added with a SIMPLE-BALANCE or POSTING-AMOUNT of type NIL or for a BALANCE to be added to an AMOUNT that is not a POSTING-AMOUNT."))

(defgeneric nplus (augend addend)
  (:documentation "Broadly similar to PLUS, except the AUGEND must be either a BALANCE or SIMPLE-BALANCE, and it is destructively modified. This is for keeping track of running totals."))

