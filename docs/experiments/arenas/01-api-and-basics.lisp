;;; Probe SBCL's arena support: is it compiled in, what's the API, does it work?
(require :sb-introspect)

(format t "~&SBCL ~a~%" (lisp-implementation-version))
(format t "~&:system-tlabs in *features*? ~a~%"
        (and (member :system-tlabs *features*) t))
(format t "~&arena-related features: ~s~%"
        (remove-if-not (lambda (f) (search "ARENA" (string f))) *features*))

(format t "~&~%=== arglists ===~%")
(dolist (sym '(sb-vm:new-arena sb-vm:destroy-arena sb-vm:rewind-arena
               sb-vm:switch-to-arena sb-vm:unuse-arena
               sb-vm:arena-bytes-used sb-vm:arena-bytes-wasted
               sb-vm:find-containing-arena sb-vm:c-find-heap->arena
               sb-vm:show-heap->arena sb-vm:points-to-arena))
  (format t "~&  ~a~30t~s~%" sym
          (handler-case (sb-introspect:function-lambda-list sym)
            (error (e) (format nil "<~a>" (type-of e))))))

(format t "~&~%=== macros ===~%")
(dolist (sym '(sb-vm:with-arena sb-vm:without-arena sb-vm:in-same-arena))
  (format t "~&  ~a~30t~s~%" sym
          (handler-case (sb-kernel:%fun-lambda-list (macro-function sym))
            (error (e) (format nil "<~a>" (type-of e))))))

(format t "~&~%=== live test ===~%")
(handler-case
    (let ((a (sb-vm:new-arena (* 4 1024 1024))))
      (format t "~&  created: ~s~%" a)
      (format t "~&  bytes-used before: ~a~%" (sb-vm:arena-bytes-used a))
      (let (escaped)
        (sb-vm:with-arena (a)
          (let ((x (make-list 1000 :initial-element 42)))
            (setf escaped x)
            (format t "~&  allocated 1000-elt list inside arena~%")
            (format t "~&  containing arena of that list: ~s~%"
                    (sb-vm:find-containing-arena
                     (sb-kernel:get-lisp-obj-address x)))))
        (format t "~&  bytes-used after alloc: ~a~%" (sb-vm:arena-bytes-used a))
        (format t "~&  bytes-wasted: ~a~%" (sb-vm:arena-bytes-wasted a))
        ;; the dangerous case: a binding outside the arena still referencing
        ;; arena memory. does SBCL detect it?
        (format t "~&  escaped ref still readable pre-rewind: ~a~%"
                (length escaped))
        (sb-vm:rewind-arena a)
        (format t "~&  bytes-used after rewind: ~a~%" (sb-vm:arena-bytes-used a)))
      (sb-vm:destroy-arena a)
      (format t "~&  destroyed ok~%"))
  (error (e) (format t "~&  ERROR: ~a: ~a~%" (type-of e) e)))

(format t "~&~%=== heap->arena leak detection ===~%")
(handler-case
    (let ((a (sb-vm:new-arena (* 1024 1024)))
          (holder (list nil)))
      (sb-vm:with-arena (a)
        (setf (car holder) (make-list 10)))   ; heap cons now points into arena
      (format t "~&  c-find-heap->arena result: ~s~%" (sb-vm:c-find-heap->arena a))
      (sb-vm:destroy-arena a))
  (error (e) (format t "~&  ERROR: ~a: ~a~%" (type-of e) e)))

(format t "~&~%done~%")
