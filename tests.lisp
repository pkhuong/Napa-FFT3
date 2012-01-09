(defun test-pairs (n)
  (assert (= 1 (logcount n)))
  (let* ((fwd (compile nil
                       `(lambda (vec start twiddle)
                          (declare (type complex-sample-array 
                                         vec twiddle)
                                   (type index start))
                          twiddle
                          ,(gen-dif n))))
         (inv (compile nil
                       `(lambda (vec start twiddle)
                          (declare (type complex-sample-array 
                                         vec twiddle)
                                   (type index start))
                          twiddle
                          ,(gen-dit n :scale (/ 1d0 n)))))
         (iota (iota n))
         (copy (copy-seq iota)))
    (funcall fwd copy 0
             (make-twiddle n))
    (funcall inv copy 0
             (make-twiddle n -1d0))
    (reduce #'min (map '(simple-array double-float 1)
                       (lambda (x y)
                         (declare (type complex-sample x y))
                         (abs (- x y)))
                       iota copy))))
