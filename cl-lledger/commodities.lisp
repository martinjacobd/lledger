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
   (display-position
    :type     (member nil :prepositive :postpositive) 
    :initarg  :display-position
    :accessor commodity-display-position)
   (thousands-separator
    :type     (member nil #\, #\.)
    :initarg  :thousands-separator
    :accessor commodity-thousands-separator)
   (decimal-separator
    :type     (member nil #\. #\,)
    :initarg  :decimal-separator
    :accessor commodity-decimal-separator)
   (negative-style
    :type     (member nil :minus-sign :parentheses)
    :initarg  :negative-style
    :accessor commodity-negative-style)
   (ticker
    :type     string
    :initarg  :ticker
    :accessor commodity-ticker)
   (valuation-style
    :type     (member nil :face-value :market-value :cost-basis)
    :initarg  :valuation-style
    :accessor commodity-valuation-style)
   (market-balancing-account
    :type     (or null account list)
    :initarg  :balancing-account
    :accessor commodity-market-balancing-account)
   (p/l-balancing-account
    :type     (or null account list)
    :initarg  :p/l-balancing-account
    :accessor commodity-p/l-balancing-account))
  (:documentation "A commodity defines the basic 'unit' of an amount. The NAME is that by which the commodity is known, and a string NOTE may be attached. The next four paramaters are about the display of an amount specified in a commodity. For any of them, they may be unspecified and thus decided situationally according to the principle of subsidiarity.

If DISPLAY-POSITION is :PREPOSITIVE, the name of the commodity is displayed before the numerical amount, for instance. THOUSANDS-SEPARATOR and DECIMAL-SEPARATOR are useful for specifying amounts in the 'European' convention ($1.000,00) or the 'American' one ($1,000.00).

The TICKER slot is useful for specifying that certain commodities might have external names that are useful for modules that download prices from the Internet or similar. (For instance, I might wish to simply call my commodity ozAg but my software to download the prices might prefer for it to be specified as XAGUSD.)

If MARKET-BALANCING-ACCOUNT is specified, that is the account to which discrepancies between the current value and the cost-basis will be posted before those discrepancies are realized. If a list is specified, it must have exactly two elements: The account to which the difference will be posted if it is a credit difference and the account to  which the difference is posted if it is a debit difference. P/L-BALANCING-ACCOUNT is similar to MARKET-BALANCING-ACCOUNT but used when the gains or losses are realized. (Normally, the correct account to which to post this discrepancy is specified in the transaction itself, but it is useful if the commodity often has a difference between the cost and the value. Thus, I may specify that any discrepancies between the cost and the value of silver, for instance, should be posted to the 'EXPENSES:PRECIOUS METALS PREMIUMS' account or similar.)")
  (:default-initargs :ticker                   ""
		     :thousands-separator      nil
		     :decimal-separator        nil
		     :negative-style           nil
		     :display-position         nil
		     :market-balancing-account nil
		     :p/l-balancing-account    nil))

(defclass item-class (commodity)
  ((attributes
    :type     list
    :initarg  :attributes
    :accessor item-class-attributes))
  (:documentation "An ITEM-CLASS is a special kind of commodity which has ATTRIBUTES (a list of strings) a la object orientation. (E.g., I may wish to define a coins ITEM-CLASS to keep track of my coins, where I would keep track of their precious metals content, mintage year, and condition, and use these attributes in a specially-written valuation function.)")
  (:default-initargs :valuation-fn nil :balancing-account nil))

(defclass item ()
  ((name
    :type     string
    :initarg  :name
    :accessor item-name)
   (class
    :type     item-class
    :initarg  :class
    :accessor item-type)
   (attributes
    :type     list
    :initarg  :attributes
    :accessor item-attributes)
   (cost
    :type     (or null amount)
    :initarg  :cost
    :accessor item-cost)
   (market-balancing-account
    :type     (or null account list)
    :initarg  :balancing-account
    :accessor item-market-balancing-account)
   (p/l-balancing-account
    :type     (or null account list)
    :initarg  :p/l-balancing-account
    :accessor item-p/l-balancing-account))
  (:documentation "Individual items are of class ITEM. A NAME may help identify it (if date or lot isn't enough), but it isn't necessary. It must have an item-class CLASS. It may have a list of attributes corresponding to that class, ATTRIBUTES. COST represents the cost basis of the account where the item currently resides. Item cost is usually specified; however, if it is not (if only the cost of the lot is specified and the cost of the item cannot be inferred), it is an error for that item to be posted outside of its lot. MARKET-BALANCING-ACCOUNT and P/L-BALANCING-ACCOUNT, behave similarly for those to the commodity. They override those in the CLASS, if any.")
  (:default-initargs :name                     ""
		     :attributes               nil
		     :cost                     nil
		     :market-balancing-account nil
		     :p/l-balancing-account    nil))

(defclass lot ()
  ((date-posted
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
  (:documentation "Lots represent collections of either items or a single quantity with a cost basis. They may have  NAME if it is helpful for disambiguation purposes, but they must have a COST. The DATE-POSTED represents the date at which it was last updated (posted to an account, had part of it sold or divided, etc.)")
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
  (:documentation "Holds individual amount lots (such as '40 MSFT @ $10'. Here, 40 MSFT would be the AMOUNT and $10 the COST)."))
