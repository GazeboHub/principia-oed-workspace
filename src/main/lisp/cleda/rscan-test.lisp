
(in-package #:cl-user)

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defpackage #:cleda-user
    (:use #:cleda #:cl)))


(in-package #:cleda)

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
;; (assuming one calculation per milisecond, for illustration)
;; = 0.20983265046 day
;; shortly more than 5 hours
;; ^ the maximum time to calcluate the set of resistors Re for an
;; equivalent resistance Rt given Sr of length n, maximum  number of
;; resistors in circuit 'a' ... assuming that any useful subset of Sr
;; can be arbitrary computed for an equivalent Rt


;; assorted top-level forms

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
