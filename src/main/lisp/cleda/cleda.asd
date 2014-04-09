;; cleda.asd					-*-lisp-*-

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

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (require '#:asdf))


(defpackage #:cleda-system
  (:use #:asdf #:cl))


(in-package #:cleda-system)

(defsystem #:cleda
  :version "1.0.3"
  :license "Eclipse Public License 1.0"
  :componnents
  ((:file "cleda-package")
   (:file "types"
          :depends-on ("cleda-package"))
   (:file "cleda"
         :depends-on ("types" "cleda-package"))
   (:file "rscan"
          :depends-on ("cleda" "types" "cleda-package"))

   ))