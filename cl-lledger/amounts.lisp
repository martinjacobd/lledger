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
    :type     (member :debit :credit)
    :initarg  :type
    :accessor posting-amount-type))
  (:documentation "For the amounts in postings (either from transactions or on an account), which must either be debits or credits. It is inferred from context if not supplied. A :DEBIT or :CREDIT TYPE is specified to determine the column the amount goes in in reports.")
  (:default-initargs :type :debit))

(defclass simple-balance (quantity)
  ((amounts
    :type     list
    :initarg  :amounts
    :accessor simple-balance-amounts)
   (type
    :type     (member nil :debit :credit)
    :initarg  :type
    :accessor simple-balance-type))
  (:documentation "Occasionally, a quantity is required to have amounts in multiple commodities, simple balance is used for this purpose."))

(defclass balance (quantity)
  ((debit-balance
    :type     list
    :accessor balance-debit-balance
    :initarg  :debit-balance)
   (credit-balance
    :type     list
    :accessor balance-credit-balance
    :initarg  :credit-balance))
  (:documentation "A balance object will carry both the debits and credits side of a transaction or account."))

(defgeneric quantity-copy (quantity)
  (:documentation "Makes a deep copy of the quantity QUANTITY."))

(defmethod quantity-copy ((quantity amount))
  (make-instance 'amount
		 :commodity (amount-commodity quantity)
		 :value     (amount-value     quantity)))

(defmethod quantity-copy ((quantity posting-amount))
  (make-instance 'posting-amount
		 :commodity (amount-commodity    quantity)
		 :value     (amount-value        quantity)
		 :type      (posting-amount-type quantity)))

(defmethod quantity-copy ((quantity simple-balance))
  (make-instance 'simple-balance
		 :type    (simple-balance-type quantity)
		 :amounts (mapcar #'quantity-copy (simple-balance-amounts quantity))))

(defmethod quantity-copy ((quantity balance))
  (make-instance 'balance
		 :debit-balance  (mapcar #'quantity-copy (balance-credit-balance quantity))
		 :credit-balance (mapcar #'quantity-copy (balance-credit-balance quantity))))

(defgeneric quantity-negate (quantity)
  (:documentation "Returns a new quantity of the same type as QUANTITY with the amount or amounts negated, preserving the TYPE characteristic, if any."))

(defmethod quantity-negate ((quantity amount))
  (make-instance 'amount
		 :commodity (amount-commodity quantity)
		 :value     (- (amount-value  quantity))))

(defmethod quantity-negate ((quantity posting-amount))
  (make-instance 'posting-amount
		 :commodity (amount-commodity    quantity)
		 :value     (- (amount-value     quantity))
		 :type      (posting-amount-type quantity)))

(defmethod quantity-negate ((quantity simple-balance))
  (make-instance 'simple-balance
		 :type (simple-balance-type quantity)
		 :amounts (mapcar #'quantity-negate (simple-balance-amounts quantity))))

(defmethod quantity-negate ((quantity balance))
  (make-instance 'balance
		 :credit-balance (mapcar #'quantity-negate (balance-credit-balance quantity))
		 :debit-balance  (mapcar #'quantity-negate (balance-debit-balance  quantity))))

(defgeneric quantity-plus (quantity-a quantity-b)
  (:documentation "Adds together two quantities. If QUANTITY-A and QUANTITY-B are both of type AMOUNT and have the same COMMODITY, then an AMOUNT of that commodity is returned. If they have differing commodities, then a SIMPLE-BALANCE of TYPE nil is returned with QUANTITY-A and QUANTITY-B copied to the AMOUNTS slot. If QUANTITY-A and QUANTITY-B are both of type POSTING-AMOUNT and have the same COMMODITY, then a POSTING-AMOUNT of that commodity is returned, with TYPE being either :DEBIT or :CREDIT depending on which allows the returned amount to have a positive value (or the TYPE of QUANTITY-A in the case they cancel). A SIMPLE-BALANCE and AMOUNT or POSTING-AMOUNT add together as you might expect, with quantities of the same commodity condensed appropriately. It is, however, an error for a POSTING-AMOUNT and SIMPLE-BALANCE of different TYPE to be added together. Similarly, a BALANCE and a SIMPLE-BALANCE add as expected, as do a BALANCE and POSTING-AMOUNT. It is an error for a BALANCE to be added to an AMOUNT that is not a POSTING-AMOUNT."))

(defun add-amount-lists (list-a list-b)
  (declare (type list list-a list-b))
  "Adds together two lists, LIST-A and LIST-B, whose members are all AMOUNTS and returns a new list."
  (check-type list-a list)
  (check-type list-b list)
  (assert (and (reduce #'(lambda (a b) (and a b))
		       (mapcar #'(lambda (item) (typep item 'amount))
			       list-a)
		       :initial-value t)
	       (reduce #'(lambda (a b) (and a b))
		       (mapcar #'(lambda (item) (typep item 'amount))
			       list-b)
		       :initial-value t))
	  (list-a list-b)
	  "LIST-A and LIST-B must contain only objects of type AMOUNT or POSTING-AMOUNT.")
  (let ((commodities (union (mapcar #'amount-commodity list-a)
			    (mapcar #'amount-commodity list-b))))
    (loop :for commodity :in commodities
	  :unless (= (+ (aif (find commodity list-a :key #'amount-commodity)
			     (amount-value it)
			     0)
			(aif (find commodity list-b :key #'amount-commodity)
			     (amount-value it)
			     0))
		     0) ; discard empty amounts
	  :collect (make-instance 'amount
				  :commodity commodity
				  :value (+ (aif (find commodity list-a :key #'amount-commodity)
						 (amount-value it)
						 0)
					    (aif (find commodity list-b :key #'amount-commodity)
						 (amount-value it)
						 0))))))

(defun sub-amount-lists (list-a list-b)
  (declare (type list list-a list-b))
  "Subtracts two lists, LIST-B from LIST-A, whose members are all AMOUNTS, and returns the new list."
  (check-type list-a list)
  (check-type list-b list)
  (assert (and (reduce #'(lambda (a b) (and a b))
		       (mapcar #'(lambda (item) (typep item 'amount))
			       list-a)
		       :initial-value t)
	       (reduce #'(lambda (a b) (and a b))
		       (mapcar #'(lambda (item) (typep item 'amount))
			       list-b)
		       :initial-value t))
	  (list-a list-b)
	  "LIST-A and LIST-B must contain only objects of type AMOUNT or POSTING-AMOUNT.")
  (let ((commodities (union (mapcar #'amount-commodity list-a)
			    (mapcar #'amount-commodity list-b))))
    (loop :for commodity :in commodities
	  :unless (= (- (aif (find commodity list-a :key #'amount-commodity)
			     (amount-value it)
			     0)
			(aif (find commodity list-b :key #'amount-commodity)
			     (amount-value it)
			     0))
		     0) ; discard empty amounts
	  :collect (make-instance 'amount
				  :commodity commodity
				  :value (- (aif (find commodity list-a :key #'amount-commodity)
						 (amount-value it)
						 0)
					    (aif (find commodity list-b :key #'amount-commodity)
						 (amount-value it)
						 0))))))

(defun add-amount-to-list (amount list)
  "Adds amount AMOUNT to list of amounts LIST."
  (add-amount-lists (list amount) list))

(defmacro amount-list-incf (list amount)
  "Adds AMOUNT to amount list LIST destructively. LIST must be setf-able."
  (let ((matching-item (gensym)))
    `(let ((,matching-item (find (amount-commodity ,amount) ,list :key #'amount-commodity)))
       (if ,matching-item
	   (if (= (+ (amount-value ,amount)
		     (amount-value ,matching-item))
		  0)
	       (setf ,list (remove ,matching-item ,list))
	       (setf (amount-value ,matching-item)
		     (+ (amount-value ,amount)
			(amount-value ,matching-item))))
	   (setf ,list (cons ,amount ,list))))))

(defmacro amount-list-decf (list amount)
  "Subtracts amount AMOUNT from amount list LIST destructively. LIST must be setf-able."
  (let ((matching-item (gensym)))
    `(let ((,matching-item (find (amount-commodity ,amount) ,list :key #'amount-commodity)))
       (if ,matching-item
	   (if (= (- (amount-value ,matching-item)
		     (amount-value ,amount))
		  0)
	       (setf ,list (remove ,matching-item ,list))
	       (setf (amount-value ,matching-item)
		     (- (amount-value ,matching-item)
			(amount-value ,amount))))
	   (setf ,list (cons ,amount ,list))))))

(defmacro amount-lists-addf (list-a list-b)
  "Adds the amounts in LIST-B to those in LIST-A destructively. LIST-A must be setf-able."
  (let ((amounts-to-remove (gensym))
	(matching-amount   (gensym))
	(amount            (gensym)))
    `(let ((,amounts-to-remove nil))
       (dolist (,amount ,list-b)
	 (let ((,matching-amount (find (amount-commodity ,amount) ,list-a :key #'amount-commodity)))
	   (if ,matching-amount
	       (if (= (+ (amount-value ,amount)
			 (amount-value ,matching-amount))
		      0)
		   (push ,matching-amount ,amounts-to-remove)
		   (setf (amount-value ,matching-amount)
			 (+ (amount-value ,amount)
			    (amount-value ,matching-amount))))
	       (setf ,list-a (cons ,amount ,list-a))))))))
	
(defmethod quantity-plus ((quantity-a amount) (quantity-b amount))
  (if (eql (amount-commodity quantity-a)
	   (amount-commodity quantity-b))
      (make-instance 'amount
		     :commodity (amount-commodity quantity-a)
		     :value     (+ (amount-value quantity-a)
				   (amount-value quantity-b)))
      (make-instance 'simple-balance
		     :type nil
		     :amounts (list (quantity-copy quantity-a)
				    (quantity-copy quantity-b)))))

(defmethod quantity-plus ((quantity-a posting-amount) (quantity-b posting-amount))
  (if (eql (amount-commodity quantity-a)
	   (amount-commodity quantity-b))
      (if (eql (posting-amount-type quantity-a)
	       (posting-amount-type quantity-b))
	  (make-instance 'posting-amount
			 :type  (posting-amount-type quantity-a)
			 :value (+ (amount-value quantity-a)
				   (amount-value quantity-b))
			 :commodity (amount-commodity quantity-a))
	  (make-instance 'posting-amount
			 :type  (if (> (amount-value quantity-a)
				       (amount-value quantity-b))
				    (posting-amount-type quantity-a)
				    (posting-amount-type quantity-b))
			 :value (abs (- (amount-value quantity-a)
					(amount-value quantity-b)))
			 :commodity (amount-commodity quantity-a)))
      (if (eql (posting-amount-type quantity-a)
	       (posting-amount-type quantity-b))
	  (make-instance 'simple-balance
			 :type (posting-amount-type quantity-a)
			 :amounts (list (quantity-copy quantity-a)
					(quantity-copy quantity-b)))
	  (make-instance 'balance
			 :debit-balance
			 (if (eql (posting-amount-type quantity-a) :debit)
			     (list (quantity-copy quantity-a))
			     (list (quantity-copy quantity-b)))
			 :credit-balance
			 (if (eql (posting-amount-type quantity-a) :credit)
			     (list (quantity-copy quantity-a))
			     (list (quantity-copy quantity-b)))))))

(defmethod quantity-plus ((quantity-a posting-amount) (quantity-b amount))
  (if (eql (amount-commodity quantity-a)
	   (amount-commodity quantity-b))
      (make-instance 'posting-amount
		     :type (posting-amount-type quantity-a)
		     :commodity (amount-commodity quantity-a)
		     :value (+ (amount-value quantity-a)
			       (amount-value quantity-b)))
      (make-instance 'simple-balance
		     :type (posting-amount-type quantity-a)
		     :amounts (list (quantity-copy quantity-a)
				    (quantity-copy quantity-b)))))

(defmethod quantity-plus ((quantity-a amount) (quantity-b posting-amount))
  (quantity-plus quantity-b quantity-a))
  
(defmethod quantity-plus ((quantity-a simple-balance) (quantity-b simple-balance))
  (assert (not (and (not (eql (simple-balance-type quantity-a)
			      (simple-balance-type quantity-b)))
		    (member nil (list (simple-balance-type quantity-a)
				      (simple-balance-type quantity-b)))))
	  (quantity-a quantity-b)
	  "A simple-balance of type NIL cannot be added to a simple-balance of another type.")
  (if (eql (simple-balance-type quantity-a)
	   (simple-balance-type quantity-b))
      (make-instance 'simple-balance
		     :type (simple-balance-type quantity-a)
		     :amounts (add-amount-lists (simple-balance-amounts quantity-a)
						(simple-balance-amounts quantity-b)))
      (make-instance 'balance
		     :credit-balance
		     (if (eql (simple-balance-type quantity-a) :credit)
			 (mapcar #'quantity-copy (simple-balance-amounts quantity-a))
			 (mapcar #'quantity-copy (simple-balance-amounts quantity-b)))
		     :debit-balance
		     (if (eql (simple-balance-type quantity-a) :debit)
			 (mapcar #'quantity-copy (simple-balance-amounts quantity-a))
			 (mapcar #'quantity-copy (simple-balance-amounts quantity-b))))))

(defmethod quantity-plus ((quantity-a simple-balance) (quantity-b posting-amount))
  (assert (not (eql (simple-balance-type quantity-a) nil))
	  (quantity-a)
	  "Cannot add a posting-amount to a simple-balance of type NIL.")
  (if (eql (simple-balance-type quantity-a) (posting-amount-type quantity-b))
      (make-instance 'simple-balance
		     :type (simple-balance-type quantity-a)
		     :amounts (add-amount-to-list quantity-b (simple-balance-amounts quantity-a)))
      (if (eql (simple-balance-type quantity-a) :debit)
	  (make-instance 'balance
			 :credit (list (quantity-copy quantity-b))
			 :debit  (mapcar #'quantity-copy (simple-balance-amounts quantity-a)))
	  (make-instance 'balance
			 :debit  (list (quantity-copy quantity-b))
			 :credit (mapcar #'quantity-copy (simple-balance-amounts quantity-a))))))

(defmethod quantity-plus ((quantity-a posting-amount) (quantity-b simple-balance))
  (quantity-plus quantity-b quantity-a))

(defmethod quantity-plus ((quantity-a simple-balance) (quantity-b amount))
  (assert (not (typep quantity-b 'posting-amount))
	  (quantity-b)
	  "INTERNAL PACKAGE ERROR: QUANTITY-PLUS method not properly combining.")
  (make-instance 'simple-balance
		 :type (simple-balance-type quantity-a)
		 :amounts (add-amount-to-list quantity-b (simple-balance-amounts quantity-a))))


(defmethod quantity-plus ((quantity-a amount) (quantity-b simple-balance))
  (quantity-plus quantity-b quantity-a))

(defmethod quantity-plus ((quantity-a balance) (quantity-b balance))
  (make-instance 'balance
		 :credit-balance
		 (add-amount-lists (balance-credit-balance quantity-a)
				   (balance-credit-balance quantity-b))
		 :debit
		 (add-amount-lists (balance-debit-balance quantity-a)
				   (balance-debit-balance quantity-b))))

(defmethod quantity-plus ((quantity-a balance) (quantity-b simple-balance))
  (assert (not (eql (simple-balance-type quantity-b) nil))
	  (quantity-a quantity-b)
	  "Cannot add simple-balance of type NIL to balance.")
  (make-instance 'balance
		 :credit-balance
		 (if (eql (simple-balance-type quantity-b) :credit)
		     (add-amount-lists (simple-balance-amounts quantity-b)
				       (balance-credit-balance quantity-a))
		     (mapcar #'quantity-copy (balance-credit-balance quantity-a)))
		 :debit-balance
		 (if (eql (simple-balance-type quantity-b) :debit)
		     (add-amount-lists (simple-balance-amounts quantity-b)
				       (balance-debit-balance quantity-a))
		     (mapcar #'quantity-copy (balance-debit-balance quantity-a)))))

(defmethod quantity-plus ((quantity-a simple-balance) (quantity-b balance))
  (quantity-plus quantity-b quantity-a))

(defmethod quantity-plus ((quantity-a balance) (quantity-b posting-amount))
  (make-instance 'balance
		 :credit-balance
		 (if (eql (posting-amount-type quantity-b) :credit)
		     (add-amount-to-list quantity-b
					 (balance-credit-balance quantity-a))
		     (mapcar #'quantity-copy (balance-credit-balance quantity-a)))
		 :debit-balance
		 (if (eql (simple-balance-type quantity-b) :debit)
		     (add-amount-to-list quantity-b
					 (balance-debit-balance quantity-a))
		     (mapcar #'quantity-copy (balance-debit-balance quantity-a)))))

(defmethod quantity-plus ((quantity-a posting-amount) (quantity-b balance))
  (quantity-plus quantity-b quantity-a))

(defmethod quantity-plus ((quantity-a balance) (quantity-b amount))
  ;; since the preceding method doesn't call CALL-NEXT-METHOD,
  ;; we know that quantity-b is not a POSTING-AMOUNT
  (assert (not (typep quantity-b 'posting-amount))
	  (quantity-b)
	  "INTERNAL PACKAGE ERROR: methods for QUANTITY-PLUS wrongly combined.")
  (error "Cannot add an AMOUNT that is not a POSTING-AMOUNT to a BALANCE."))

(defmethod quantity-plus ((quantity-a amount) (quantity-b balance))
  (quantity-plus quantity-b quantity-a))

(defgeneric quantity-minus (quantity-a quantity-b)
  (:documentation "Returns a quantity which is the result of subtracting QUANTITY-B from QUANTITY-A. Rules for the acceptable combinations of types of QUANTITY-A and QUANTITY-B are the same as those of QUANTITY-PLUS."))

(defmethod quantity-minus ((quantity-a quantity) (quantity-b quantity))
  (quantity-plus quantity-a (quantity-negate quantity-b)))

(defgeneric quantity-times (quantity factor)
  (:documentation "Returns a new quantity of the same kind as QUANTITY with all of its amounts multiplied by FACTOR, which may be any non-imaginary number."))

(defmethod quantity-times :around ((quantity quantity) factor)
  (check-type factor (and number (not complex)))
  (call-next-method))

(defmethod quantity-times ((quantity amount) factor)
  (make-instance 'amount
		 :commodity (amount-commodity quantity)
		 :value (rationalize (* factor (amount-value quantity)))))

(defmethod quantity-times ((quantity posting-amount) factor)
  (make-instance 'posting-amount
		 :commodity (amount-commodity quantity)
		 :value (rationalize (* factor (amount-value quantity)))
		 :type (posting-amount-type quantity)))

(defmethod quantity-times ((quantity simple-balance) factor)
  (make-instance 'simple-balance
		 :type (simple-balance-type quantity)
		 :amounts (mapcar #'(lambda (amount)
				      (quantity-times amount factor))
				  (simple-balance-amounts quantity))))

(defmethod quantity-times ((quantity balance) factor)
  (make-instance 'balance
		 :credit-balance (mapcar #'(lambda (amount)
					     (quantity-times amount factor))
					 (balance-credit-balance quantity))
		 :debit-balance  (mapcar #'(lambda (amount)
					     (quantity-times amount factor))
					 (balance-debit-balance quantity))))


(defgeneric balance-incf (augend addend)
  (:documentation "Broadly similar to QUANTITY-PLUS, except the AUGEND must be either a BALANCE or SIMPLE-BALANCE, and it is destructively modified. This is for keeping track of running totals. Note that the order of arguments matters here, as well as the fact that the addend cannot be a BALANCE if AUGEND is a SIMPLE-BALANCE. The other combinations outlined in (documentation #'QUANTITY-PLUS 'function), however, work, provided that the balance or simple-balance is the AUGEND."))

(defmethod balance-incf ((augend simple-balance) (addend amount))
  (amount-list-incf (simple-balance-amounts augend) addend))

(defmethod balance-incf :around ((augend simple-balance) (addend posting-amount))
  (if (eql (simple-balance-type augend)
	   (posting-amount-type addend))
      (call-next-method)
      (call-next-method augend (make-instance 'posting-amount
					      :value (- (amount-value addend))
					      :commodity (amount-commodity addend)
					      :type (simple-balance-type augend)))))

(defmethod balance-incf ((augend simple-balance) (addend simple-balance))
  (amount-lists-addf (simple-balance-amounts augend)
		     (if (eql (simple-balance-type augend)
			      (simple-balance-type addend))
			 (simple-balance-amounts addend)
			 (mapcar #'(lambda (amount)
				     (make-instance 'amount
						    :value (- (amount-value amount))
						    :commodity (amount-commodity amount)))
				 (simple-balance-amounts addend)))))

(defmethod balance-incf ((augend balance) (addend amount))
  (error "Cannot use bare AMOUNT types that are not POSTING-AMOUNTs with BALANCE type AUGENDs in BALANCE-INCF."))

(defmethod balance-incf ((augend balance) (addend posting-amount))
  (if (eql (posting-amount-type addend) :credit)
      (amount-list-incf (balance-credit-balance augend) (amount-value addend))
      (amount-list-incf (balance-debit-balance  augend) (amount-value addend))))

(defmethod balance-incf ((augend balance) (addend simple-balance))
  (assert (member (simple-balance-type addend) `(,:credit ,:debit))
	  (addend)
	  "Cannot add NIL-type SIMPLE-BALANCE to BALANCE.")
  (if (eql (simple-balance-type addend) :credit)
      (amount-lists-addf (balance-credit-balance augend) (simple-balance-amounts addend))
      (amount-lists-addf (balance-debit-balance  augend) (simple-balance-amounts addend))))

(defmethod balance-incf ((augend balance) (addend balance))
  (amount-lists-addf (balance-credit-balance augend) (balance-credit-balance addend))
  (amount-lists-addf (balance-debit-balance  augend) (balance-debit-balance  addend)))

(defgeneric balance-decf (minuend subtrahend)
  (:documentation "Destructively subtracts the SUBTRAHEND from the MINUEND. Rules for the acceptable types are the same as those of BALANCE-INCF."))

(defmethod balance-decf ((minuend quantity) (subtrahend quantity))
  (balance-incf minuend (quantity-negate subtrahend)))

(defgeneric balance-total (balance &optional positive-type)
  (:documentation "Calculates the total balance of BALANCE. It calculates debits - credits if POSITIVE-TYPE is :DEBIT and credits - debits if POSITIVE-TYPE is :CREDIT. POSITIVE-TYPE is assumed to be :DEBIT if not provided, just as in ledger-cli. A SIMPLE-BALANCE is returned with TYPE POSITIVE-TYPE and AMOUNTS as outlined above."))

(defmethod balance-total ((balance balance) &optional (positive-type :DEBIT))
  (check-type positive-type (member :debit :credit))
  (make-instance 'simple-balance
		 :type positive-type
		 :amounts 
		 (sub-amount-lists
		  (if (eql positive-type :debit)
		      (balance-debit-balance  balance)
		      (balance-credit-balance balance))
		  (if (eql positive-type :debit)
		      (balance-credit-balance balance)
		      (balance-debit-balance  balance)))))

(defgeneric amount-print (amount &key
				   positive-type
				   display-position
				   thousands-separator
				   decimal-separator
				   negative-style)
  (:documentation "Prints the value of AMOUNT according to its commodity representation or the amounts given as more general representation defaults given as arguments, if the commodity representation preferances are null."))

(defmethod amount-print ((amount amount) &key
					   positive-type
					   display-position
					   thousands-separator
					   decimal-separator
					   negative-style)
  (let ((commodity (amount-commodity amount)))
    (let ((display-position    (choose-specific-alternative (commodity-display-position commodity)
							    display-position))
	  (thousands-separator (choose-specific-alternative (commodity-thousands-separator commodity)
							    thousands-separator))
	  (decimal-separator   (choose-specific-alternative (commodity-decimal-separator commodity)
							    decimal-separator))
	  (negative-style      (choose-specific-alternative (commodity-negative-style commodity)
							    negative-style))))
    (format t "")))
