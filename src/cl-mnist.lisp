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

(defun fetch (&optional (fetcher #'dex:fetch))
  (loop :for file :in +mnist-files+
        :for pathname := (format nil "~A~A" +dataset-directory+ file)
        :do (funcall fetcher (format nil "~A~A" +mnist-base+ file)
                     (format nil "~A~A" +dataset-directory+ file)
                     :if-exists :supersede)
        :collect pathname))

(defun extract (gzip-pathname &optional (extract t))
  (let ((pathname
         (car (uiop:split-string gzip-pathname :separator "." :max 2))))
    (when extract
      (with-open-file (out pathname :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create)
        (chipz:decompress out :gzip gzip-pathname)))
    pathname))

(defun extracts (pathnames &optional (extract t))
  (mapcar (lambda (x) (extract x extract)) pathnames))

(defun load-labels (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (fast-io:with-fast-input (buff nil in)
      (fast-io:read32-be buff) ; Discard magic number.
      (let* ((size (fast-io:read32-be buff))
             (result (make-array size :element-type '(unsigned-byte 8))))
        (assert (= size (fast-io:fast-read-sequence result buff)))
        result))))
