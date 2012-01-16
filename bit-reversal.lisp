(in-package "NAPA-FFT.GEN")

(defun emit-small-bit-reverse (n &aux
                                   (width (lb n)))
  (assert (power-of-two-p n))
  `(progn
     ,@(loop
         for i below n
         for rev = (bit-reverse-integer i width)
         when (< i rev)
         collect
         `(rotatef (aref vec (+ start ,i))
                   (aref vec (+ start ,rev))))))

;; another way to z-order the bottom bits:
(defun generate-outward-seq (seed most least)
  (let ((values (make-array (* least least)
                            :adjustable t
                            :fill-pointer 0)))
    (labels ((rec (seed most least)
               (if (plusp least)
                   (dotimes (i 2)
                     (dotimes (j 2)
                       (rec (+ seed
                               (* i most)
                               (* j least))
                            (ash most 1)
                            (ash least -1))))
                   (vector-push-extend seed values))))
      (rec seed most least)
      (make-array (length values)
                  :initial-contents values))))

(defun z-order-words (pairs &optional (mask -1))
  (flet ((interleave (x y)
           (let ((x   (logand x mask))
                 (y   (logand y mask))
                 (acc 0)
                 (mul 0))
             (dotimes (i 32 acc)
               (let ((x (ldb (byte 1 i) x))
                     (y (ldb (byte 1 i) y)))
                 (setf acc (logior (ash (+ (* 2 x) y)
                                        mul)
                                   acc)
                       mul (+ 2 mul)))))))
    (let ((interleaved (map 'simple-vector
                            (lambda (pair)
                              (cons (interleave (car pair)
                                                (cdr pair))
                                    pair))
                            pairs)))
      (sort interleaved #'< :key #'car)
      (map-into interleaved #'cdr interleaved))))

(defun generate-leaf-reverse (outer inner)
  (let ((to-swap '())
        (total    (+ outer inner)))
    (dotimes (i (ash 1 outer))
      (let ((ri (bit-reverse-integer i outer)))
        (dotimes (j (ash 1 outer))
          (let* ((rj (bit-reverse-integer j outer))
                 (a (+ (ash i  total) j))
                 (b (+ (ash rj total) ri)))
            (when (< a b)
              (push (cons a b) to-swap))))))
    `(progn
       ,@(loop
           for (a . b) across (z-order-words to-swap
                                             (1- (ash 1 outer)))
           collect `(let ((a (aref vec (+ middle ,a)))
                          (b (aref vec (+ middle ,b))))
                      (setf (aref vec (+ middle ,a)) b
                            (aref vec (+ middle ,b)) a))))))

(defun generate-leaf-reverse-swap (outer inner)
  (let ((to-swap '())
        (total    (+ outer inner)))
    (dotimes (i (ash 1 outer))
      (let ((ri (bit-reverse-integer i outer)))
        (dotimes (j (ash 1 outer))
          (let* ((rj (bit-reverse-integer j outer))
                 (a  (+ (ash i  total) j))
                 (b  (+ (ash rj total) ri)))
            (push (cons a b) to-swap)))))
    `(progn
       ,@(loop
           for (a . b) across (z-order-words to-swap
                                             (1- (ash 1 outer)))
           collect
           `(let ((a (aref vec (+ middle-1 ,a)))
                  (b (aref vec (+ middle-2 ,b))))
              (setf (aref vec (+ middle-1 ,a)) b
                    (aref vec (+ middle-2 ,b)) a))))))

(defun generate-large-reversal (outer inner)
  (let ((to-swap '()))
    (dotimes (i (ash 1 inner))
      (let ((ri (bit-reverse-integer i inner)))
        (when (<= i ri)
          (push (cons i ri) to-swap))))
    (let* ((ordered (z-order-words to-swap
                                   (1- (ash 1 (ceiling inner 2)))))
           (data (make-array (* 2 (length ordered))
                             :element-type '(unsigned-byte 32))))
      (loop for i upfrom 0 by 2
            for (a . b) across ordered
            do (setf (aref data      i) (ash a outer)
                     (aref data (1+ i)) (ash b outer)))
      `(flet ((rev (middle)
                (declare (type index middle))
                ,(generate-leaf-reverse outer inner)
                vec)
              (swap (middle-1 middle-2)
                (declare (type index middle-1 middle-2))
                ,(generate-leaf-reverse-swap outer inner)
                vec))
         (declare (ignorable #'swap #'rev))
         ,(if (> (length ordered) 8)
              `(let ((data ,data))
                 (loop for i of-type index below ,(length data) by 2 do
                   (let ((middle-1 (aref data i))
                         (middle-2 (aref data (1+ i))))
                     (if (= middle-1 middle-2)
                         (rev  (+ start middle-1))
                         (swap (+ start middle-1)
                               (+ start middle-2)))))
                 vec)
              `(progn
                 ,@(loop for i below (length data) by 2
                         collect
                         (let ((middle-1 (aref data i))
                               (middle-2 (aref data (1+ i))))
                           (if (= middle-1 middle-2)
                               `(rev  (+ start ,middle-1))
                               `(swap (+ start ,middle-1)
                                      (+ start ,middle-2)))))))))))

(defvar *outer-width* 3)
(defvar *max-small-bit-reverse* 256)

(defun gen-bit-reversal (n)
  (assert (power-of-two-p n))
  (if (<= n *max-small-bit-reverse*)
      (emit-small-bit-reverse n)
      (let* ((width (lb n))
             (outer (min *outer-width*
                         (floor width 2)))
             (inner (- width (* 2 outer))))
        (generate-large-reversal outer inner))))
