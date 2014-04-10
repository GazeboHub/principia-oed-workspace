;; cleda-package.lisp

#|

Eclipse Public License - v 1.0

The accompanying program is provided under the terms of this Eclipse
Public License ("agreement"). Any use, reproduction or distribution of
the program constitutes recipient's acceptance of this agreement.


The source code accompanying this file should contain a verbatim copy
of the Eclipse Public License 1.0

The Eclipse Public License 1.0 is available at the URL:
   http://www.eclipse.org/legal/epl-v10.html


|#

(in-package #:cl-user)

(defpackage #:cleda
  (:use #:cl)

  (:export
   ;; types.lisp
   #:rating
   ;; cleda.lisp
   #:opt-r
   #:par-r-2
   #:par-r-n
   #:i-ab
   #:v-ab
   ;; rscan.lisp
   ;;; 'R' structure class
   #:r
   #:r-p
   #:make-r
   #:r-rating
   #:r-tolerance
   #:r-rating-min
   #:r-rating-max
   #:make-r
   ;;; functional interface
   #:make-rset
   #:search-par-r-2
   #:search-par-r-n
   ))

(defpackage #:cleda-user
  (:use #:cleda #:cl))
