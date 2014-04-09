;; cleda.lisp -- Common Lisp EDA tools 1.0.2

#|

## Notes

### See also

[Jakub Higersberger's unit-formula system]
 (https://github.com/Ramarren/unit-formula)

|#

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

(deftype rating ()
  "Resistor rating value type"
  '(or single-float fixnum))

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


;;; #


(defstruct (r
            (:constructor %make-r
                (rating tolerance rating-min rating-max)))
  (rating 0 :type rating)
  ;; ^ FIXME : the only single-float rating in the use case is 2.2
  (tolerance 0 :type single-float) ;; FIXME: use single-float instead
  (rating-min 0 :type real)
  (rating-max 0 :type real))


(defun make-r (rating tolerance)
  "Make an R object - a resistor of rating RATING with a specified
manfucatured rating tolerance TOLERANCE

TOLERANCE should be provided as a SINGLE-FLOAT value"
  (declare (type real rating rating)
           (type single-float tolerance)
           (values r &optional))
  (let* ((r-val (opt-r rating))
         (diff (* r-val tolerance))
         (min (opt-r (- r-val diff)))
         (max (opt-r (+ r-val diff))))
    (declare (type rating r-val min max)
             (type single-float diff))
    (%make-r r-val tolerance min max)))

(defmethod print-object ((object r) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ohm (~s%)"
            (r-rating object)
            (opt-r (* (r-tolerance object) 100)))))

;; (make-r 1E+03 .05)

(defun make-rset (tolerance &rest ratings)
  (declare (values simple-vector &optional))
  (let ((buff (make-array 8 :element-type t
                            :fill-pointer 0
                            :adjustable t)))
    (dolist (rtg ratings (coerce buff 'simple-vector ))
      (let ((r (make-r rtg tolerance)))
        (vector-push-extend r buff 8)))))

;; (make-rset 0.05 1E+03 10E+03 100E+03)


(defun search-par-r-2 (rt rset)
  (declare (type real rt)
           (type simple-vector rset)
           (values simple-vector &optional))
  ;; FIXME: Optimize math for the common FIXNUM rating
  (let ((len (length rset))
        (buff (make-array 8 :element-type t
                            :fill-pointer 0
                            :adjustable t)))
    (declare (type (mod #.(1- array-dimension-limit)) len)
             (type (vector t) buff))
    (dotimes (n1 len (coerce buff 'simple-vector))
      (let* ((r1-ob (svref rset n1))
             (r1-r (r-rating r1-ob)))
        (declare (type rating r1-r))
        (block r2
          (dotimes (n2 len)
            (locally (declare (inline par-r-2))
              (let* ((r2-ob (svref rset n2))
                     (r2-r (r-rating r2-ob))
                     (r-eq (par-r-2 r1-r r2-r)))
              (declare (type rating r2-r)
                       (type real r-eq))
                (when (= r-eq rt)
                  (let ((bucket (vector r1-ob r2-ob)))
                    (vector-push-extend bucket buff)
                    (return-from r2 nil)))))))))))




#| Use case:

 (defparameter *rset*
   ;; referring to RadioShack catalog item 2710003
   (make-rset
    0.05
    ;; 30 each :
    1E+03 10E+03 100
    ;; 10 each :
    1 10 100 120 150 220 330 470
    560 1.5E+03 2.2E+03 3.3E+03 4.7E+03 15E+03
    22E+03 47E+03 220E+03 470E+03 1E+06 10E+06
    ;; 5 each:
    2.2 15 22 33 39 47 51 68 82
    180 270 390 510 680 820 1.2E+03
    1.8E+03 2.7E+03 3E+03 3.9E+03 5.1E+03 5.6E+03 6.8E+03
    8.2E+03 12E+03 18E+03 27E+03 33E+03 39E+03 51E+03
    56E+03 68E+03 82E+03 120E+03 150E+03 180E+03
    270E+03 330E+03 1.5E+06 2.2E+06
    3.3E+06 4.7E+06))

;; (length *rset*)
;; => 65 ;; n = 65
;; for a = 5, calculate n^1 + n^2 + ... n^a
;;

 (defun expsum (n a)
   (declare (type fixnum n a)
            (values fixnum &optional))
   (let ((i 0))
     (declare (type fixnum i))
     (dotimes (an a i)
       (setq i (+ i (expt n an))))))

;;; (expsum 65 5) => 18129541

;; (/ 18129541 60.0)
;; with 18129541 a measure of miliseconds
;; = 0.20983265046 day
;; shortly more than 5 hours
;; ^ the maximum time to calcluate the set of resistors Re for an
;; equivalent resistance Rt given Sr of length n, maximum  number of
;; resistors in circuit 'a' ... assuming that any useful subset of Sr
;; can be arbitrary computed for an equivalent Rt


;;

 (map 'list #'(lambda (bucket)
                (cons (r-rating (svref bucket 0))
                      (r-rating (svref bucket 1))))
      (search-par-r-2 20 *rset*))
 ;;  => #((220 . 22) (22 . 220))
 ;; ^ one of the few examples for which this trivial two-resistor
 ;; search would appear to be of any use
 ;;
 ;; Note that it returns to "equivalent sets" however

 (map 'list #'identity
      (search-par-r-2 0.005952 *rset*))
 ;; => NIL



|#


(defun search-par-r-n (rt rset max-n)
  (declare (type real rt)
           (type simple-vector rset)
           (type fixnum max-n)
           (values simple-vector &optional))
  ;; TO DO
  (error "Not implemented")
  )
