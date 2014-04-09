;; cleda.lisp -- Common Lisp EDA tools 1.0.1

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

;;; #

(defun par-r (r1 r2)
  (declare (type real r1 r2)
           (values real &optional))
  "Calculate equivalent resistance of two resistors in parallel"
  (/ (* r1 r2) (+ r1 r2)))


;;; #

(defun i-ab  (it r1 r2)
  "Calculate current for each circuit branch in an element of two
parallel resistances r1, r2 creating a current divider in a circuit
of current il"
  (declare (type real it r1 r2)
           (values real real &optional))
  (let* ((r_eq (par-r r1 r2))
         (ir (* it r_eq)))
    (declare (type real r_eq ir))
    (values (/ ir r1)
            (/ ir r2))))

(defun v-ab (vt r1 r2)
  "Calculate voltage differential across each resistor of a series of
two resistances r1, r2 creating a voltage divider in a circuit
of current vt"
  (declare (type real vt r1 r2)
           (values real real &optional))
  (let* ((r_eq (+ r1 r2))
         (vr (/ vt r_eq)))
    (declare (type real r_eq vr))
    (values (* r1 vr) (* r2 vr))))


;;; #


(deftype rating ()
  '(or single-float fixnum))

(defstruct (r
            (:constructor %make-r
                (rating tolerance rating-min rating-max)))
  (rating 0 :type rating)
  ;; ^ FIXME : the only single-float rating in the use case is 2.2
  (tolerance 0 :type real) ;; FIXME: use single-float instead
  (rating-min 0 :type real)
  (rating-max 0 :type real))

(defun opt-r (r)
  (declare (type rating r)
           (values rating &optional))
  (let ((r-trunc (truncate r)))
    (declare (type fixnum r-trunc))
    (cond
      ((= r r-trunc) r-trunc)
      (t r))))

;; (opt-r 1.0E+03)
;; (opt-r 1)
;; (opt-r 2.2)


(defun make-r (rating tolerance)
  ;; tolerance: as ratio (not percent)
  (declare (type real rating tolerance)
           (values r &optional))
  (let* ((r-val (opt-r rating))
         (diff (* r-val tolerance))
         (min (opt-r (- r-val diff)))
         (max (opt-r (+ r-val diff))))
    (declare (type rating r-val diff min max ))
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


(defun search-par-r (rt rset)
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
            (locally (declare (inline par-r))
              (let* ((r2-ob (svref rset n2))
                     (r2-r (r-rating r2-ob))
                     (r-eq (par-r r1-r r2-r)))
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

(map 'list #'(lambda (bucket)
               (cons (r-rating (svref bucket 0))
                     (r-rating (svref bucket 1))))
     (search-par-r 20 *rset*))
;;  => #((220 . 22) (22 . 220))
;; ^ one of the few examples for which this trivial two-resistor
;; search would appear to be of any use
;;
;; Note that it returns to "equivalent sets" however

(map 'list #'identity
     (search-par-r 0.005952 *rset*))
;; => NIL



|#