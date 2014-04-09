;; rscan.lisp

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


(in-package #:cleda)

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


#| Unit testing / use case: refer to rscan-test.lisp |#


(defun search-par-r-n (rt rset max-n)
  (declare (type real rt)
           (type simple-vector rset)
           (type fixnum max-n)
           (values simple-vector &optional))
  ;; TO DO
  (error "Not implemented")
  )
