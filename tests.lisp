(in-package "NAPA-FFT.TESTS")

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

(defun make-scaled-fwd (n scale)
  (let ((twiddle (make-twiddle n))
        (fun     (compile
                  nil
                  `(lambda (vec start twiddle)
                     (declare (type complex-sample-array 
                                    vec twiddle)
                              (type index start))
                     twiddle
                     ,(gen-dif n :scale scale)))))
    (lambda (vec)
      (funcall fun vec 0 twiddle))))

(defun make-scaled-inv (n scale)
  (let ((twiddle (make-twiddle n -1d0))
        (fun     (compile
                  nil
                  `(lambda (vec start twiddle)
                     (declare (type complex-sample-array 
                                    vec twiddle)
                              (type index start))
                     twiddle
                     ,(gen-dit n :scale scale)))))
    (lambda (vec)
      (funcall fun vec 0 twiddle))))

(defvar *fancy-in-order* t)

(defun get-fancy-fwd (n scale)
  (get-fft n
           :forward t
           :scale (cond ((= scale 1)
                         nil)
                        ((= scale (/ 1d0 n))
                         :inv)
                        (t :sqrt))
           :in-order *fancy-in-order*))

(defun get-fancy-inv (n scale)
  (get-fft n
           :forward nil
           :scale (cond ((= scale 1)
                         nil)
                        ((= scale (/ 1d0 n))
                         :inv)
                        (t :sqrt))
           :in-order *fancy-in-order*))

(defun test-pairs (n &key (fwd-scale 1d0)
                       (fwd 'make-scaled-fwd)
                       (inv 'make-scaled-inv))
  (assert (power-of-two-p n))
  (let* ((fwd (funcall fwd n fwd-scale))
         (inv (funcall inv n (/ 1d0 n fwd-scale)))
         (iota (random-vector n))
         (copy (copy-seq iota)))
    (funcall fwd copy)
    (funcall inv copy)
    (delta iota copy)))

(defun run-pairs (max &key (fwd 'make-scaled-fwd)
                           (inv 'make-scaled-inv))
  (format t "scale: 1~%")
  (loop for i upto max
        do (format t "~A: ~A~%" i
                   (/ (test-pairs (ash 1 i)
                                  :fwd fwd
                                  :inv inv)
                      (max i 1))))
  (format t "scale: sqrt~%")
  (loop for i upto max
        do (format t "~A: ~A~%" i
                   (/ (test-pairs
                       (ash 1 i)
                       :fwd-scale (/ (sqrt (float (ash 1 i) 1d0)))
                       :fwd fwd
                       :inv inv)
                      (max i 1))))
  (format t "scale: 1/n~%")
  (loop for i upto max
        do (format t "~A: ~A~%" i
                   (/ (test-pairs
                       (ash 1 i)
                       :fwd-scale (/ (float (ash 1 i) 1d0))
                       :fwd fwd
                       :inv inv)
                      (max i 1)))))

(defun make-dummy-window (n)
  (map-into (make-array n :element-type 'double-float)
            (lambda ()
              (+ (random 1d0) 1d0))))

(defun apply-window-inv (vec window)
  (declare (type complex-sample-array vec)
           (type (simple-array double-float 1) window))
  (map-into vec #'/ vec window))

(defun make-windowed-fwd (n window)
  (let ((fun (compile nil
                      `(lambda (vec start window window-start twiddle)
                         (declare (type complex-sample-array 
                                        vec twiddle)
                                  (type (simple-array double-float 1)
                                        window)
                                  (type index start window-start))
                         twiddle window window-start
                         ,(gen-dif n :scale  (/ (sqrt (float n 1d0)))
                                     :window (and window 'window)))))
        (twiddle (make-twiddle n)))
    (lambda (vec window)
      (funcall fun vec 0 window 0 twiddle))))

(defun make-windowed-inv (n window)
  (let ((fun (compile nil
                      `(lambda (vec start window window-start twiddle)
                         (declare (type complex-sample-array 
                                        vec twiddle)
                                  (type (simple-array double-float 1)
                                        window)
                                  (type index start window-start))
                         twiddle window window-start
                         ,(gen-dit n :scale  (/ (sqrt (float n 1d0)))
                                     :window (and window 'window)))))
        (twiddle (make-twiddle n -1d0)))
    (lambda (vec window)
      (funcall fun vec 0 window 0 twiddle))))

(defun get-fancy-windowed-fwd (n window)
  (if window
      (get-windowed-fft n 'float
                        :scale :sqrt
                        :in-order *fancy-in-order*)
      (let ((fun (get-fancy-fwd n (/ (sqrt (float n 1d0))))))
        (lambda (vec window)
          window
          (funcall fun vec)))))

(defun get-fancy-windowed-inv (n window)
  (if window
      (let ((fun (get-windowed-fft n 'float
                                   :forward nil
                                   :scale :sqrt
                                   :in-order *fancy-in-order*))
            (rev (get-reverse n 'double-float)))
        (if *fancy-in-order*
            (lambda (vec window)
              (funcall rev window)
              (funcall fun vec window))
            fun))
      
      (let ((fun (get-fancy-inv n (/ (sqrt (float n 1d0))))))
        (lambda (vec window)
          window
          (funcall fun vec)))))

(defun test-window (n &key (window (make-dummy-window n))
                        (window-fwd t)
                        (window-inv t)
                        (fwd 'make-windowed-fwd)
                        (inv 'make-windowed-inv))
  (assert (power-of-two-p n))
  (let* ((fwd   (funcall fwd n window-fwd))
         (inv   (funcall inv n window-inv))
         (iota (random-vector n))
         (copy (copy-seq iota)))
    (when window-fwd
      (apply-window-inv copy window))
    (funcall fwd copy window)
    (when window-inv
      (apply-window-inv copy window))
    (funcall inv copy (copy-seq window))
    (delta iota copy)))

(defun run-windows (max
                    &key (fwd-maker 'make-windowed-fwd)
                         (inv-maker 'make-windowed-inv)
                    &aux (window (make-dummy-window (ash 1 max))))
  (dolist (fwd '(t nil))
    (dolist (inv (if fwd '(t nil) '(t)))
      (format t "window: ~A ~A~%"
              (if fwd "T" "F")
              (if inv "T" "F"))
      (loop for i upto max do
        (format t "~A: ~A~%" i (/ (test-window (ash 1 i)
                                               :fwd fwd-maker
                                               :inv inv-maker
                                               :window window
                                               :window-fwd fwd
                                               :window-inv inv)
                                  (max i 1)))))))

(defun test-offset (n)
  (let ((funs (mapcar
               (lambda (x)
                 (compile nil x))
               `((lambda (vec start window window-start)
                   (declare (type complex-sample-array 
                                  vec)
                            (type (simple-array double-float 1)
                                  window)
                            (type index start window-start))
                   (let ((twiddle ,(make-twiddle n)))
                     twiddle
                     ,(gen-dif n :window 'window)))
                 (lambda (vec start window window-start)
                   (declare (type complex-sample-array 
                                  vec)
                            (type (simple-array double-float 1)
                                  window)
                            (type index start window-start))
                   (let ((twiddle ,(make-twiddle n -1d0)))
                     twiddle
                     ,(gen-dit n :window 'window))))))
        (diff 0d0))
    (dolist (fun funs diff)
      (let* ((window (make-dummy-window n))
             (offset-window (make-array (+ n 7)
                                        :element-type 'double-float))
             (iota (random-vector n))
             (offset-iota (make-vector (+ n 13)))
             (ref  (copy-seq iota)))
        (replace offset-window window :start1 7)
        (replace offset-iota   iota   :start1 13)
        (funcall fun ref 0 window 0)
        (dolist (offset-window-p '(t nil))
          (multiple-value-bind (window window-start)
              (if offset-window-p
                  (values offset-window 7)
                  (values window        0))
            (dolist (offset-data-p (if offset-window-p
                                       '(t nil)
                                       '(t)))
              (multiple-value-bind (data start)
                  (if offset-data-p
                      (values offset-iota 13)
                      (values iota        0))
                (setf data (copy-seq data))
                (funcall fun
                         data start
                         window window-start)
                (let ((data (subseq data start)))
                  (setf diff (max diff (delta ref data))))))))))))

(defun run-offsets (max)
  (format t "Offset: should always have exactly 0 error~%")
  (loop for i upto max do
    (format t "~A: ~A~%" i (test-offset (ash 1 i)))))
