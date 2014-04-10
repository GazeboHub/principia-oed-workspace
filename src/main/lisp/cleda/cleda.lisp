;; cleda.lisp -- Common Lisp EDA tools 1.0.2

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

#|

## Notes

### See also

[Jakub Higersberger's unit-formula system]
 (https://github.com/Ramarren/unit-formula)

|#


(in-package #:cleda)


(defun opt-r (r)
  "For R of type RATING, if R can be represented as a FIXNUM value,
return the truncation of R, else return R"
  (declare (type real r)
           (values rating &optional))
  (let ((r-trunc (truncate r)))
    (declare (type fixnum r-trunc))
    (cond
      ((= r r-trunc) r-trunc)
      ((typep r 'single-float) r)
      (t (coerce r 'single-float)))))

;; (opt-r 1.0E+03)
;; (opt-r 1)
;; (opt-r 2.2)
;; (opt-r 10000/11)


;;; #

(defun par-r-2 (r1 r2)
  (declare (type rating r1 r2)
           (values rating &optional))
  "Calculate equivalent resistance of two resistors in parallel"
  (opt-r (/ (* r1 r2) (+ r1 r2))))


(defun par-r-n (&rest rn)
  (declare (values rating &optional))
  (opt-r
   (/ 1 (apply #'+
               (mapcar  #'(lambda (r)
                            (declare (type rating r))
                            (/ 1 r))
                        rn)))))

;; (par-r-n 10 10)
;; => 5

;; (par-r-n 10 10 10 10 10)
;; => 2


(defun par-r-n-2 (r1 &rest rm)
  (declare (type rating r1)
           (values rating &optional))
  (cond
    (rm
     (let ((r-eq (par-r-2 r1 (car rm)))
           (rm-rest (cdr rm)))
       (cond
         (rm-rest
          (apply #'par-r-n-2 r-eq rm-rest))
         (t r-eq))))
    (t r1)))

;; (par-r-n-2 10 10)
;; => 5

;; (par-r-n-2 10 10 10 10 10)
;; => 2





;;; #

(defun i-ab  (it r1 r2)
  "Calculate current for each circuit branch in an element of two
parallel resistances r1, r2 creating a current divider in a circuit
of current il"
  (declare (type rating it r1 r2)
           (values rating rating &optional))
  (let* ((r_eq (par-r-2 r1 r2))
         (ir (* it r_eq)))
    (declare (type rating r_eq ir))
    (values (opt-r (/ ir r1))
            (opt-r (/ ir r2)))))

(defun v-ab (vt r1 r2)
  "Calculate voltage differential across each resistor of a series of
two resistances r1, r2 creating a voltage divider in a circuit
of current vt"
  (declare (type rating vt r1 r2)
           (values rating rating &optional))
  (let* ((r_eq (+ r1 r2))
         (vr (/ vt r_eq)))
    (declare (type rating r_eq vr))
    (values
     (opt-r (* r1 vr))
     (opt-r (* r2 vr)))))
