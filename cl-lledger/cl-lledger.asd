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

(defsystem "cl-lledger"
  :description "hello-lisp: a sample Lisp system."
  :version "development"
  :author "Jacob Martin <martinjacobd@gmail.com>"
  :licence "GPLv3"
  :depends-on ("cl-ppcre")
  :components ((:file "package")
               (:file "utilities")
	       (:file "accounting")
               (:file "amounts")
	       (:file "commodities")))
