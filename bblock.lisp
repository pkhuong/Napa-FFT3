;;; Basic block compiler.

(defstruct (vval
            (:constructor %make-vval))
  name ;; Var name
  type ;; type
  spills ;; available spill slots
  dests  ;; mandatory (write-once) destination slots
  status ;; "current" locations
  reads  ;; sorted list of locations of ops that read this var
  )

(defun vval (type &key (stem "VAR"))
  (%make-vval :name (gensym (coerce stem 'string))
              :type type
              :spills '()
              :dests '()
              :status '()
              :reads '()))

(defstruct (op
            (:constructor %make-op))
  body
  args
  deps
  writes)

(defun %op (result-types body args)
  (let ((results (mapcar (lambda (type)
                           (vval type))
                         result-types)))
    (%make-op :body body
              :args args
              :deps (remove-duplicates (remove-if-not #'vval-p args))
              :writes results)))

(defvar *ops*)

(defun op (result-types body args)
  (let ((op (%op result-types body args)))
    (vector-push-extend op *ops*)
    (values-list (op-writes op))))

(defvar *vector*)

(defun @ (i)
 (aref *vector* i))

(defun (setf @) (value i)
  (when (vval-p value)
    (pushnew i (vval-spills value)))
  (setf (aref *vector* i) value))

(defun initialize-vector (type)
  (dotimes (i (length *vector*) *vector*)
    (setf (@ i) (vval type :stem (format nil "~A[~A]" 'vec i)))))

(defun note-writes ()
  (dotimes (i (length *vector*) *vector*)
    (let ((value (@ i)))
      (when (vval-p value)
        (pushnew i (vval-dests value))))))

(defmacro with-vector ((n &key (type '(complex double-float))
                            (maxlive 14))
                       &body body)
  `(let* ((*vector* (make-array ,n))
          (*ops* (make-array 16 :fill-pointer 0 :adjustable t))
          initial-vector)
     (initialize-vector ',type)
     (setf initial-vector (copy-seq *vector*))
     (locally
         ,@body)
     (note-writes)
     (emit-code
      (insert-spill *ops*
                    initial-vector
                    *vector*
                    ,maxlive)
      '(vec))))

(defmacro bblock ((&rest bindings) &body body)
  (let ((body `(locally ,@body)))
    (loop for (names types form) in (reverse bindings)
          do (setf body
                   `(multiple-value-bind ,names ,form
                        (declare ,@(mapcar (lambda (type name)
                                             `(type ,type ,name))
                                           types names)
                                 (ignorable ,@names))
                      ,body))
          finally (return body))))

(defstruct load-op
  var idx)

(defstruct store-op
  var idx)

(defun emit-code (ops body)
  (let ((clauses
          (loop for op across ops
                collect
                (etypecase op
                  (op
                   `(,(mapcar #'vval-name (op-writes op))
                     ,(mapcar #'vval-type (op-writes op))
                     (,(op-body op)
                       ,@(mapcar (lambda (x)
                                   (if (vval-p x)
                                       (vval-name x)
                                       x))
                                 (op-args op)))))
                  (load-op
                   (let ((var (load-op-var op)))
                     `((,(vval-name var)) (,(vval-type var))
                       (aref vec ,(load-op-idx op)))))
                  (store-op
                   `(() () (setf (aref vec ,(store-op-idx op))
                                 ,(let ((var (store-op-var op)))
                                    (if (vval-p var)
                                        (vval-name var)
                                        var)))))))))
    `(bblock ,clauses
       ,@body)))

(defun annotate-uses (ops)
  (loop for op across ops
        for i upfrom 0
        do (dolist (dep (op-deps op))
             (setf (vval-status dep) '())
             (pushnew i (vval-reads dep))))
  (loop for op across ops
        do (dolist (write (op-writes op))
             (setf (vval-status write) '()
                   (vval-reads write)
                   (nreverse (vval-reads write)))))
  ops)

(defun insert-spill (ops initial-vector final-vector maxlive)
  (annotate-uses ops)
  (loop for val across initial-vector
        for i upfrom 0
        do (when (vval-p val)
             (push i (vval-status val))))
  (let ((new  (make-array (length ops)
                          :adjustable t
                          :fill-pointer 0))
        (live (make-array maxlive :initial-element nil)))
    (labels ((force-spills (n)
               (let ((vars (sort (delete nil
                                         (copy-seq live))
                                 #'>
                                 :key (lambda (x)
                                        (or (first (vval-reads x))
                                            most-positive-fixnum)))))
                 (loop
                   repeat n
                   for var across vars
                   do
                   (nsubstitute nil var live)
                   (emit-stores var
                                (or (vval-dests var)
                                    (list (or (first (vval-spills var))
                                              (error "Can't spill this.")))))
                   (setf (vval-status var)
                         (delete (vval-name var)
                                 (vval-status var))))))
             (emit-stores (val &optional (dests (vval-dests val)))
               (dolist (dest dests)
                 (unless (member dest (vval-status val))
                   (vector-push-extend
                    (make-store-op :var val
                                   :idx dest)
                    new)
                   (push dest (vval-status val)))))
             (kill (vals)
               (dolist (val vals)
                 (pop (vval-reads val))
                 (when (null (vval-reads val))
                   (emit-stores val)
                   (setf (vval-status val)
                         (remove (vval-name val)
                                 (vval-status val)))
                   (nsubstitute nil val live))))
             (ensure-live (vals &key (load t))
               (let* ((free (count nil live))
                      (to-load (count-if (lambda (val)
                                           (not (member (vval-name val)
                                                        (vval-status val))))
                                         vals)))
                 (when (> to-load free)
                   (force-spills (- to-load free)))
                 (dolist (val vals)
                   (let ((name (vval-name val))
                         (status (vval-status val)))
                     (when (member name status)
                       (go skip))
                     (let ((pos (position nil live)))
                       (assert pos)
                       (when load
                         (assert status)
                         (vector-push-extend
                          (make-load-op :var val
                                        :idx (first status))
                          new))
                       (setf (aref live pos) val)
                       (push name (vval-status val))))
                   skip)))
             (insert-write (val i)
               (etypecase val
                 (vval
                  (let ((vals (list val)))
                    (unless (every (lambda (dest)
                                     (member dest (vval-status val)))
                                   (vval-dests val))
                      (ensure-live vals)
                      (kill vals))))
                 (t
                  (vector-push-extend
                   (make-store-op :var val
                                  :idx i)
                   new)))))
      (loop for op across ops
            for i upfrom 0
            do (ensure-live (op-deps op))
               (kill (op-deps op))
               (vector-push-extend op new)
               (ensure-live (op-writes op) :load nil))
      (loop for val across final-vector
            for i upfrom 0
            do (insert-write val i))
      new)))
