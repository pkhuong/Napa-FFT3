(defun emit-raw32-file (file data &optional (max 1d0))
  (with-open-file (s file :direction :output :element-type '(signed-byte 32)
                          :if-exists :supersede)
    (write-sequence (map-into (make-array (length data) :element-type '(signed-byte 32))
                              (lambda (data)
                                (let ((x (/ (float (realpart data) 1d0)
                                            max)))
                                  (floor (* x (1- (ash 1 31))))))
                              data)
                    s)
    file))

(defun read-raw32-file (file &optional n (max 1d0))
  (with-open-file (s file :element-type '(signed-byte 32))
    (let* ((n   (or n
                    (file-length s)))
           (seq (make-array n :element-type '(signed-byte 32))))
      (read-sequence seq s)
      (let ((scale (expt (* 2d0 max) -31)))
        (declare (type double-float scale))
        (map 'napa-fft:real-sample-array
             (lambda (x)
               (* x scale))
             seq)))))

(defun impulse (i n &optional (value 1d0))
  (let ((vec (make-array n :element-type 'napa-fft:complex-sample
                           :initial-element (complex 0d0 0d0))))
    (dolist (i (if (listp i) i (list i)) vec)
      (setf (aref vec i) (complex (float value 1d0))))))

(defun noise (n &optional (range .5d0))
  (let ((2range (* 2 range)))
    (map-into (make-array n :element-type 'napa-fft:complex-sample)
              (lambda ()
                (complex (- (random 2range) range))))))

(defun m+ (x y &optional (scale .5d0))
  (map 'napa-fft:complex-sample-array
       (lambda (x y)
         (* scale (+ x y)))
       x y))

(defun amplitude-clamp-fraction (vector fraction)
  (let ((limit (* (reduce #'max vector :key #'abs)
                  fraction)))
    (map 'napa-fft:complex-sample-array
         (lambda (x)
           (if (< (abs x) limit)
               (complex 0d0)
               x))
         vector)))

(defun amplitude-clamp-k (vector k)
  (let* ((n      (length vector))
         (values (make-array n)))
    ;; this would be a heap or a quickselect if I cared
    (dotimes (i n)
      (setf (aref values i) (cons (abs (aref vector i)) i)))
    (sort values #'> :key #'car)
    (let ((result (make-array n
                              :element-type 'napa-fft:complex-sample
                              :initial-element (complex 0d0))))
      (loop repeat k
            for (nil . i) across values
            do (setf (aref result i) (aref vector i))
            finally (return result)))))

(defun energy (x)
  (declare (type napa-fft:complex-sample-array x)
           (optimize speed))
  (let ((acc 0d0))
    (declare (type double-float acc))
    (map nil (lambda (x)
               (let ((r (realpart x))
                     (i (imagpart x)))
                 (incf acc (+ (* r r) (* i i)))))
         x)
    acc))

(defun filter-chunks (vector filter)
  (declare (type napa-fft:real-sample-array vector filter))
  (let* ((destination (make-array (length vector)
                                  :element-type 'napa-fft:complex-sample
                                  :initial-element (complex 0d0)))
         (chunk-size  (length filter))
         (half-size   (truncate chunk-size 2))
         (chunk       (make-array chunk-size
                                  :element-type 'napa-fft:complex-sample))
         (filter      (napa-fft:bit-reverse filter)))
    (declare (optimize speed))
    (loop for i below (length vector) by half-size
          for end = (min (length vector) (+ i chunk-size))
          do (fill chunk (complex 0d0))
             (loop for dst upfrom 0
                   for src from i below end
                   do (setf (aref chunk dst) (complex (aref vector src))))
             (let ((old-energy (energy chunk)))
               (napa-fft:windowed-fft chunk half-size
                                      chunk-size
                                      :window-fn 'napa-fft:triangle
                                      :dst chunk :in-order nil)
               (napa-fft:ifft chunk
                              :dst chunk :in-order nil
                              :window filter)
               (let* ((new-energy (energy chunk))
                      (scale      (* .5d0 (sqrt (/ old-energy
                                                   new-energy)))))
                 (declare (type double-float scale))
                 (loop for src upfrom 0
                     for dst from i below end
                     do (incf (aref destination dst)
                              (* scale (aref chunk src))))))
          finally (return destination))))
