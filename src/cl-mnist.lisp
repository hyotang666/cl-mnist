(in-package :cl-user)

(defpackage :cl-mnist
  (:use :cl)
  (:export #:load-mnist))

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

(defun extract (gzip-pathname &optional force)
  (let ((pathname
         (car (uiop:split-string gzip-pathname :separator "." :max 2))))
    (when (or force (not (probe-file pathname)))
      (with-open-file (in gzip-pathname :element-type '(unsigned-byte 8))
        (with-open-file (out pathname :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :supersede
                         :if-does-not-exist :create)
          (chipz:decompress out :gzip in))))
    pathname))

(defun extracts (pathnames &optional force)
  (mapcar (lambda (x) (extract x force)) pathnames))

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
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
              (with-standard-io-syntax (print result out)))))
        (with-open-file (in (make-pathname :type "lisp" :defaults pathname))
          (with-standard-io-syntax
           (setf result (read in)
                 size (array-dimension result 0)))))
    (values ;; Modifying...
            (if one-hot
                (let ((array (make-array (list size 10) :element-type 'bit)))
                  (dotimes (x size array)
                    (setf (aref array x (aref result x)) 1)))
                result)
            size 1 8)))

(defun label-slurper (pathname &key one-hot)
  (let ((stream (open pathname :element-type '(unsigned-byte 8))))
    ;; Skip magic number.
    (assert (file-position stream 4))
    (let ((count (nibbles:read-ub32/be stream)))
      (values
        (slurp:make-slurper stream
                            (if one-hot
                                (lambda (stream)
                                  (let ((vector
                                         (make-array 10 :element-type 'bit)))
                                    (setf (aref vector (read-byte stream)) 1)
                                    vector))
                                'read-byte))
        count 1 8))))

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
                              :direction :output
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
    (values ;; Modifying...
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
                  :element-type '(unsigned-byte 8)))
            items size 16)))

(defun image-slurper (pathname &key normalize flatten)
  (let* ((stream (open pathname :element-type '(unsigned-byte 8)))
         (count
          (progn ; Skip magic number and number of images.
           (assert (file-position stream 4))
           (nibbles:read-ub32/be stream)))
         (row (nibbles:read-ub32/be stream))
         (col (nibbles:read-ub32/be stream))
         (size (* row col)))
    (labels ((slurper (key)
               (lambda (stream)
                 (let ((vector
                        (make-array size :element-type '(unsigned-byte 8))))
                   (assert (= size (read-sequence vector stream)))
                   (funcall key vector))))
             (normalize (vector)
               (map `(array ,*read-default-float-format* (*))
                    (lambda (x) (/ x 255.0)) vector))
             (matrix-maker (element-type key)
               (lambda (vector)
                 (make-array (list row col)
                             :element-type element-type
                             :displaced-to (funcall key vector)))))
      (values
        (slurp:make-slurper stream
                            (if normalize
                                (if flatten
                                    (slurper #'normalize)
                                    (slurper
                                      (matrix-maker *read-default-float-format*
                                                    #'normalize)))
                                (if flatten
                                    (slurper #'identity)
                                    (slurper
                                      (matrix-maker '(unsigned-byte 8)
                                                    #'identity)))))
        count size 16))))

(declaim
 (ftype (function
         (&key (:force boolean) (:normalize boolean) (:flatten boolean)
          (:one-hot-label boolean) (:slurp boolean))
         cons)
        load-mnist))

(defun make-plist (pathname category data count size offset)
  (cond
   ((search "train" (pathname-name pathname))
    (list (read-from-string (format nil ":train-~A" category)) data
          (read-from-string (format nil ":train-~A-count" category)) count
          (read-from-string (format nil ":train-~A-size" category)) size
          (read-from-string (format nil ":train-~A-offset" category)) offset))
   ((search "t10k" (pathname-name pathname))
    (list (read-from-string (format nil ":test-~A" category)) data
          (read-from-string (format nil ":test-~A-count" category)) count
          (read-from-string (format nil ":test-~A-size" category)) size
          (read-from-string (format nil ":test-~A-offset" category)) offset))
   (t (error "Unknown pathname: ~S" pathname))))

(defun load-mnist (&key force normalize flatten one-hot-label slurp)
  "Return plist as :train-labels :train-labels-count :train-labels-size
:train-images :train-images-count :train-images-size
:test-labels :test-labels-count :test-labels-size
:test-images :test-images-count :test-images-size."
  (loop :for pathname :in (extracts (mnist-files force) force)
        :nconc (cond
                ((search "labels" (pathname-name pathname))
                 (multiple-value-call #'make-plist
                   pathname
                   :labels
                   (if slurp
                       (label-slurper pathname :one-hot one-hot-label)
                       (load-labels pathname :one-hot one-hot-label))))
                ((search "images" (pathname-name pathname))
                 (multiple-value-call #'make-plist
                   pathname
                   :images
                   (if slurp
                       (image-slurper pathname
                                      :normalize normalize
                                      :flatten flatten)
                       (load-images pathname
                                    :normalize normalize
                                    :flatten flatten))))
                (t (error "Unknown pathname: ~S" pathname)))))
