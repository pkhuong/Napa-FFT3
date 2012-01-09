(defun delta (x y)
  (declare (type complex-sample-array x y))
  (let ((max-diff 0d0))
    (declare (type double-float max-diff))
    (if (= (length x) (length y))
        (map nil (lambda (x y)
                   (let ((diff (abs (- x y))))
                     (when (> diff 0)
                       (setf max-diff (max (/ diff
                                              (max (abs x) (abs y)))
                                           max-diff)))))
             x y)
        (setf max-diff 1d9))
    max-diff))

(defun test-pairs (n &optional (fwd-scale 1d0))
  (assert (= 1 (logcount n)))
  (let* ((fwd (compile nil
                       `(lambda (vec start twiddle)
                          (declare (type complex-sample-array 
                                         vec twiddle)
                                   (type index start))
                          twiddle
                          ,(gen-dif n :scale fwd-scale))))
         (inv (compile nil
                       `(lambda (vec start twiddle)
                          (declare (type complex-sample-array 
                                         vec twiddle)
                                   (type index start))
                          twiddle
                          ,(gen-dit n :scale (/ 1d0 n fwd-scale)))))
         (iota (iota n))
         (copy (copy-seq iota)))
    (funcall fwd copy 0
             (make-twiddle n))
    (funcall inv copy 0
             (make-twiddle n -1d0))
    (delta iota copy)))

(defun run-pairs (max)
  (format t "scale: 1~%")
  (loop for i upto max
        do (format t "~A: ~A~%" i (test-pairs (ash 1 i))))
  (format t "scale: sqrt~%")
  (loop for i upto max
        do (format t "~A: ~A~%" i (test-pairs (ash 1 i)
                                              (/ (sqrt (float (ash 1 i) 1d0))))))
  (format t "scale: 1/n~%")
  (loop for i upto max
        do (format t "~A: ~A~%" i (test-pairs (ash 1 i)
                                              (/ (float (ash 1 i) 1d0))))))

(defun make-dummy-window (n)
  (map-into (make-array n :element-type 'double-float)
            (lambda ()
              (+ (random 1d0) 1d0))))

(defun apply-window-inv (vec window)
  (declare (type complex-sample-array vec)
           (type (simple-array double-float 1) window))
  (map-into vec #'/ vec window))

(defun test-window (n &key (window (make-dummy-window n))
                        (window-fwd t)
                        (window-inv t))
  (assert (= 1 (logcount n)))
  (let* ((scale (/ (sqrt (float n 1d0))))
         (fwd (compile nil
                       `(lambda (vec start window window-start twiddle)
                          (declare (type complex-sample-array 
                                         vec twiddle)
                                   (type (simple-array double-float 1)
                                         window)
                                   (type index start window-start))
                          twiddle window window-start
                          ,(gen-dif n :scale  scale
                                      :window (and window-fwd 'window)))))
         (inv (compile nil
                       `(lambda (vec start window window-start twiddle)
                          (declare (type complex-sample-array 
                                         vec twiddle)
                                   (type (simple-array double-float 1)
                                         window)
                                   (type index start window-start))
                          twiddle window window-start
                          ,(gen-dit n :scale  scale
                                      :window (and window-inv 'window)))))
         (iota (iota n))
         (copy (copy-seq iota)))
    (when window-fwd
      (apply-window-inv copy window))
    (funcall fwd copy 0 window 0
             (make-twiddle n))
    (when window-inv
      (apply-window-inv copy window))
    (funcall inv copy 0 window 0
             (make-twiddle n -1d0))
    (delta iota copy)))

(defun run-windows (max &aux (window (make-dummy-window (ash 1 max))))
  (dolist (fwd '(t nil))
    (dolist (inv (if fwd '(t nil) '(t)))
      (format t "window: ~A ~A~%"
              (if fwd "T" "F")
              (if inv "T" "F"))
      (loop for i upto max do
        (format t "~A: ~A~%" i (test-window (ash 1 i)
                                            :window window
                                            :window-fwd fwd
                                            :window-inv inv))))))
