;; cleda.lisp -- Common Lisp EDA tools 1.0.1

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

(defun par-r (r1 r2)
  (declare (type real r1 r2)
           (values real &optional))
  "Calculate equivalent resistance of two resistors in parallel"
  (/ (* r1 r2) (+ r1 r2)))


(defun i-ab  (it r1 r2)
  "Calculate current for each circuit branch in an element of two
parallel resistances r1, r2 creating a current divider in a circuit
of current il"
  (declare (type real it r1 r2)
           (values real real &optinal))
  (let* ((r_eq (par-r r1 r2))
         (ir (* it r_eq)))
    (declare (type real r_eq ilrl))
    (values (/ ir r1)
            (/ ir r2))))

(defun v-ab (vt r1 r2)
  "Calculate voltage differential across each resistor of a series of
two resistances r1, r2 creating a voltage divider in a circuit
of current vt"
  (declare (type real vt r1 r2)
           (values real real &optional))
  (let* ((r_eq (+ r1 r2))
         (vr (/ vl r_eq)))
    (declare (type real r_eq vr))
    (values (* r1 vr) (* r2 vr))))
