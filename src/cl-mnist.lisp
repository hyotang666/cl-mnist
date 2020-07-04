(in-package :cl-user)

(defpackage :cl-mnist
  (:use :cl)
  (:export))

(in-package :cl-mnist)

(defvar +mnist-base+ "http://yann.lecun.com/exdb/mnist/")

(defvar +mnist-files+
  '("train-images-idx3-ubyte.gz" "train-labels-idx1-ubyte.gz"
    "t10k-images-idx3-ubyte.gz" "t10k-labels-idx1-ubyte.gz"))

(defvar +dataset-directory+
  (ensure-directories-exist
    (make-pathname :directory (append
                                (pathname-directory
                                  (asdf:system-source-directory
                                    (asdf:find-system :cl-mnist)))
                                '("dataset")))))

(defun mnist-files (&optional force (fetcher #'dex:fetch))
  (loop :for file :in +mnist-files+
        :for pathname := (format nil "~A~A" +dataset-directory+ file)
        :if (or force (not (probe-file pathname)))
          :do (funcall fetcher (format nil "~A~A" +mnist-base+ file)
                       (format nil "~A~A" +dataset-directory+ file)
                       :if-exists :supersede)
        :collect pathname))

(defun extract (gzip-pathname &optional (extract t))
  (let ((pathname
         (car (uiop:split-string gzip-pathname :separator "." :max 2))))
    (when extract
      (with-open-file (in gzip-pathname :element-type '(unsigned-byte 8))
        (with-open-file (out pathname :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :supersede
                         :if-does-not-exist :create)
          (chipz:decompress out :gzip in))))
    pathname))

(defun extracts (pathnames &optional (extract t))
  (mapcar (lambda (x) (extract x extract)) pathnames))

(defun load-labels (pathname &key one-hot force)
  (let (result size)
    ;; Loading...
    (if (or force
            (not (probe-file pathname))
            (not (probe-file (make-pathname :type "lisp" :defaults pathname))))
        (with-open-file (in pathname :element-type '(unsigned-byte 8))
          (fast-io:with-fast-input (buff nil in)
            (fast-io:read32-be buff) ; Discard magic number.
            (setf size (fast-io:read32-be buff)
                  result (make-array size :element-type '(unsigned-byte 8)))
            (assert (= size (fast-io:fast-read-sequence result buff)))
            (with-open-file (out (make-pathname :type "lisp"
                                                :defaults pathname)
                             :if-exists :supersede
                             :if-does-not-exist :create)
              (with-standard-io-syntax (print result out)))))
        (with-open-file (in (make-pathname :type "lisp" :defaults pathname))
          (with-standard-io-syntax
           (setf result (read in)
                 size (array-dimension result 0)))))
    ;; Modifying...
    (if one-hot
        (let ((array (make-array (list size 10) :element-type 'bit)))
          (dotimes (x size array) (setf (aref array x (aref result x)) 1)))
        result)))

(defun load-images (pathname &key normalize flatten force)
  (let (result size items row col)
    ;; Loading...
    (if (or force
            (not (probe-file pathname))
            (not (probe-file (make-pathname :type "lisp" :defaults pathname))))
        (with-open-file (in pathname :element-type '(unsigned-byte 8))
          (fast-io:with-fast-input (buff nil in)
            (fast-io:read32-be buff) ; Discard magic number.
            (setf items (fast-io:read32-be buff)
                  row (fast-io:read32-be buff)
                  col (fast-io:read32-be buff)
                  size (* row col items)
                  result (make-array size :element-type '(unsigned-byte 8)))
            (assert (= size (fast-io:fast-read-sequence result buff)))
            (with-standard-io-syntax
             (with-open-file (out (make-pathname :type "lisp"
                                                 :defaults pathname)
                              :if-exists :supersede
                              :if-does-not-exist :create)
               (print items out)
               (print row out)
               (print col out)
               (print result out)))))
        (with-open-file (in (make-pathname :type "lisp" :defaults pathname))
          (with-standard-io-syntax
           (setf items (read in)
                 row (read in)
                 col (read in)
                 size (* row col)
                 result (read in)))))
    ;; Modifying...
    (if normalize
        (make-array
          (cons items
                (if flatten
                    (list (* row col))
                    (list row col)))
          :element-type *read-default-float-format*
          :displaced-to (map `(array ,*read-default-float-format* (*))
                             (lambda (x) (/ x 255.0)) result))
        (make-array
          (cons items
                (if flatten
                    (list (* row col))
                    (list row col)))
          :displaced-to result
          :element-type '(unsigned-byte 8)))))

(defun load-mnist (&key force normalize flatten one-hot-label)
  "Return plist as :train-labels :test-labels :train-images :test-images."
  (mapcan
    (lambda (pathname)
      (cond
       ((search "labels" (namestring pathname))
        (list
          (if (uiop:string-prefix-p "train" (namestring pathname))
              :train-labels
              :test-labels)
          (load-labels pathname :one-hot one-hot-label)))
       ((search "images" (namestring pathname))
        (list
          (if (uiop:string-prefix-p "train" (namestring pathname))
              :train-images
              :test-images)
          (load-images pathname :normalize normalize :flatten flatten)))
       (t (error "Unknown file name: ~S" pathname))))
    (extracts (mnist-files force))))
