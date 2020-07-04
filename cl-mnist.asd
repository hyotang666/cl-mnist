; vim: ft=lisp et
(in-package :asdf)
(defsystem "cl-mnist"
  :version
  "0.4.0"
  :depends-on
  (
   "dexador" ; http client.
   "chipz" ; Decompression gzip.
   "uiop" ; Utilities.
   "fast-io" ; file-io.
   )
  :pathname
  "src/"
  :components
  ((:file "cl-mnist")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "cl-mnist").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "cl-mnist"))))
  (append (call-next-method) '((test-op "cl-mnist.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "cl-mnist")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after ((o load-op) (c (eql (find-system "cl-mnist"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (symbol-call :jingoh.documentizer :import c)))))
