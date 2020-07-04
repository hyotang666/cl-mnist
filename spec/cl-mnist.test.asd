; vim: ft=lisp et
(in-package :asdf)
(defsystem "cl-mnist.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "cl-mnist")
  :components
  ((:file "cl-mnist"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :cl-mnist args)))