(in-package "NAPA-FFT.GEN")

(defvar *swap-block-size* 16)

(defun emit-swaps-same (pairs builder)
  (flet ((emit (pairs)
           (let ((temps (make-hash-table))
                 (swap  (make-hash-table))
                 (locs  ()))
             (map nil (lambda (pair)
                        (destructuring-bind (a . b) pair
                          (push a locs)
                          (push b locs)
                          (setf (gethash a swap) b
                                (gethash b swap) a)))
                  pairs)
             (setf locs (sort locs #'<))
             `(bblock
                  ,(loop
                     for loc in locs
                     for temp
                       = (setf (gethash loc temps)
                               (make-symbol
                                (format nil "SWAP[~A]"
                                        (hash-table-count temps))))
                     collect `((,temp) (t) ,(funcall builder loc))
                     append
                     (let ((writes '()))
                       (loop
                         (let* ((top   (car locs))
                                (other (gethash top swap))
                                (temp  (gethash other temps)))
                           (unless temp
                             (return (nreverse writes)))
                           (pop locs)
                           (push `(() ()
                                   (setf ,(funcall builder top)
                                         ,temp))
                                 writes)))))
                nil))))
    (loop with block = (max (truncate *swap-block-size* 2) 1)
          for start below (length pairs)
            by block
          for end   = (min (length pairs)
                           (+ start block))
          collect (emit (subseq pairs start end)))))

(defun emit-swaps (pairs builder1 &optional (builder2 builder1))
  (when (eql builder1 builder2)
    (return-from emit-swaps (emit-swaps-same pairs builder1)))
  (flet ((emit (pairs)
           (when (= 1 (length pairs))
             (return-from emit
               (destructuring-bind (a . b) (elt pairs 0)
                 `(let ((a ,(funcall builder1 a))
                        (b ,(funcall builder2 b)))
                    (setf ,(funcall builder1 a) b
                          ,(funcall builder2 b) b)))))
           (let ((temps (make-hash-table))
                 (pairs (sort pairs #'< :key #'car)))
             (map nil (lambda (pair)
                        (setf (gethash pair temps)
                              (make-symbol
                               (format nil "SWAP[~A]"
                                       (hash-table-count temps)))))
                  pairs)
             (let* ((last  (elt pairs
                                (1- (length pairs))))
                    (pairs (subseq pairs
                                   0
                                   (1- (length pairs))))
                    (rev   (sort (copy-seq pairs)
                                 #'< :key #'cdr)))
               `(let ,(map 'list
                       (lambda (pair)
                         (let ((tmp (gethash pair temps)))
                           `(,tmp ,(funcall builder1 (car pair)))))
                       pairs)
                  (let ,(map 'list
                         (lambda (pair)
                           (let ((tmp   (gethash pair temps))
                                 (place (funcall builder2 (cdr pair))))
                             `(,tmp (prog1 ,place
                                      (setf ,place ,tmp)))))
                         rev)
                    ,(let ((first (elt pairs 0)))
                       `(setf ,(funcall builder1 (car first))
                              ,(gethash first temps)))
                    (let ((a ,(funcall builder1 (car last)))
                          (b ,(funcall builder2 (cdr last))))
                      (setf ,(funcall builder1 (car last)) b
                            ,(funcall builder2 (cdr last)) a))
                    ,@(map 'list
                           (lambda (pair)
                             `(setf ,(funcall builder1 (car pair))
                                    ,(gethash pair temps)))
                           (subseq pairs 1))))))))
    (loop with block = (max *swap-block-size* 1)
          for start below (length pairs)
            by block
          for end   = (min (length pairs)
                           (+ start block))
          collect (emit (subseq pairs start end)))))

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

(defun emit-small-bit-reverse (n &aux
                                   (width (lb n)))
  (assert (power-of-two-p n))
  (let ((to-swap '()))
    (dotimes (i n)
      (let ((rev (bit-reverse-integer i width)))
        (when (< i rev)
          (push (cons i rev) to-swap))))
    `(progn
       ,@(emit-swaps (z-order-words to-swap
                                    (1- (ash 1 (ceiling width 2))))
                     (lambda (i)
                       `(aref vec (+ start ,i))))
       nil)))

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
       ,@(emit-swaps (z-order-words to-swap
                                    (1- (ash 1 outer)))
                     (lambda (i)
                       `(aref vec (+ middle ,i))))
       nil)))

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
       ,@(emit-swaps (z-order-words to-swap
                                    (1- (ash 1 outer)))
                     (lambda (i)
                       `(aref vec (+ middle-1 ,i)))
                     (lambda (i)
                       `(aref vec (+ middle-2 ,i))))
       nil)))

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
