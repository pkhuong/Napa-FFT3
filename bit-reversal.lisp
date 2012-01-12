(defun make-bit-reversal-table (n &optional (type 'fixnum))
  (assert (= 1 (logcount n)))
  (let ((vec (make-array n :element-type type))
        (width (lb n)))
    (dotimes (i n vec)
      (setf (aref vec i)
            (bit-reverse-integer i width)))))

(defvar *bit-reversed-octets* (make-bit-reversal-table 256 '(unsigned-byte 8)))

(defun emit-small-bit-reverse-swap (n &aux
                                        (width (lb n)))
  (assert (= 1 (logcount n)))
  `(progn
     ,@(loop
         for i below n
         for rev = (bit-reverse-integer i width)
         when (<= i rev)
         collect
         (if (= i rev)
             `(rotatef (aref vec (+ start1 ,i))
                       (aref vec (+ start2 ,i)))
             `(progn
                (rotatef (aref vec (+ start1 ,i))
                         (aref vec (+ start2 ,rev)))
                (rotatef (aref vec (+ start2 ,i))
                         (aref vec (+ start1 ,rev))))))))

(defun emit-small-bit-reverse (n &aux
                                   (width (lb n)))
  (assert (= 1 (logcount n)))
  `(progn
     ,@(loop
         for i below n
         for rev = (bit-reverse-integer i width)
         when (< i rev)
         collect
         `(rotatef (aref vec (+ start ,i))
                   (aref vec (+ start ,rev))))))

(defun emit-small-bit-reverse-copy (n &aux
                                        (width (lb n)))
  (assert (= 1 (logcount n)))
  `(progn
     ,@(loop
         for i below n
         for rev = (bit-reverse-integer i width)
         collect
         `(setf (aref dst (+ startd ,i))
                (aref src (+ starts ,rev))))))

(defun emit-radix-n-reversal (radix n)
  (assert (= 1 (logcount radix)))
  (assert (= 1 (logcount n)))
  (assert (>= n radix))
  (let ((div (truncate n radix))
        (width (lb radix)))
    `(for (,div
           (src-i starts ,radix)
           (dst-i startd))
       ,@(loop
           for i below radix
           collect `(setf (aref dst (+ dst-i
                                       ,(* (bit-reverse-integer
                                            i width)
                                           div)))
                          (aref src (+ src-i ,i)))))))

(defvar *max-small-bit-reverse* 256)
(defvar *max-reversal-radix* 16)

(defun gen-bit-reversal (n)
  (assert (= 1 (logcount n)))
  (labels ((rec (n vec tmp)
             (cond ((> n *max-small-bit-reverse*)
                    (let* ((radix (min *max-reversal-radix*
                                       (the size
                                            (/ n *max-small-bit-reverse*))))
                           (sub   (/ n radix)))
                      `(progn
                         (let ((dst ,tmp)
                               (src ,vec)
                               (startd startt)
                               (starts start))
                           (declare (type complex-sample-array dst src)
                                    (type index starts startd))
                           ,(emit-radix-n-reversal radix n))
                         (for (,radix
                               (src-i startt ,sub)
                               (dst-i start  ,sub))
                           (let ((startt dst-i)
                                 (start src-i))
                             (declare (type index start startt)
                                      (ignorable start startt))
                             ,(rec sub tmp vec))))))
                   ((eql vec 'vec)
                    (emit-small-bit-reverse n))
                   (t
                    (assert (eql tmp 'vec))
                    `(let ((src ,vec)
                           (dst ,tmp)
                           (starts start)
                           (startd startt))
                       (declare (type complex-sample-array
                                      src dst)
                                (type index starts startd))
                       ,(emit-small-bit-reverse-copy n))))))
    ;; args: dst tmp startd startt
    `(symbol-macrolet ((%unroll-count% 1)
                       (%blocking-factor% ,most-positive-fixnum))
       ,(rec n 'vec 'tmp))))
