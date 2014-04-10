
(in-package #:cleda-user)


#|

 benchmarking par-r-n / par-r-n-2

|#


#|
 1. load rscan-test.lisp - defining cleda-user::*rset*
|#

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (load "rscan-test"))

#|
 2. Initialize *r-bench*
|#

(defparameter *r-bench*
  (let ((rb-base
          (mapcar #'r-rating
                  (coerce cleda-user::*rset* 'list)))
        (rb-repro 100)
        (rb-buff))
    (dotimes (n rb-repro rb-buff)
      ;; avoiding defining a tail-recursive implementation
      ;; for this ad-hoc test bench code
      (setq rb-buff (append rb-base rb-buff)))))

#|
 3. time the computation in each function for the equivalent
    resistance of *r-bench* (trivial initial procedure))
|#

(let (times-non-tail times-tail)
  #+SBCL (sb-ext:gc)
  (setq times-non-tail
        (with-output-to-string (*trace-output*)
          (time (apply #'cleda::par-r-n cleda::*r-bench*))))
  #+SBCL (sb-ext:gc)
  (setq times-tail
        (with-output-to-string (*trace-output*)
          ;; fails - control stack exhausted
          ;; TO DO: compile with optimization for tail recursion?
          (time (apply #'cleda::par-r-n-2 cleda::*r-bench*))))

  ;; Note that the # bytes consed is reported only after the first
  ;; time call of each respective form (cf. memoization??)

  (print "Time report (non tail recursive)")
  (terpri)
  (print times-non-tail)
  (terpri 2)


  (print "Time report (tail recursive)")
  (terpri)
  (print times-tail)
  (terpri 2)

  )
