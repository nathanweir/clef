;;;; What do the structured alternatives to DESCRIBE actually return?
;;;;
;;;; hover.lisp calls (describe sym stream) and recovers everything it needs with
;;;; five regexes over SBCL's English prose -- the same anti-pattern W0 removed
;;;; from diagnostics. Every field has a structured equivalent, and the file
;;;; already reaches for two of them. Before rewriting on top of those, measure
;;;; what they give for the shapes hover actually meets.
;;;;
;;;; Run: sbcl --noinform --non-interactive --load docs/experiments/lsp/04-hover-sources.lisp

(require :sb-introspect)

(defvar *report* (make-string-output-stream))
(defun say (fmt &rest args) (apply #'format *report* fmt args) (terpri *report*))

(defun ftype-of (sym)
  "The declared or derived function type, as a readable specifier."
  (ignore-errors
   (let ((ctype (sb-int:info :function :type sym)))
     (when ctype (sb-kernel:type-specifier ctype)))))

(defun source-file-of (sym kind)
  (ignore-errors
   (let ((sources (sb-introspect:find-definition-sources-by-name sym kind)))
     (when sources
       (let ((path (sb-introspect:definition-source-pathname (first sources))))
         (when path (namestring path)))))))

(defun probe (label sym)
  (say "~&~%=== ~A : ~S ===" label sym)
  (say "  fboundp            ~S" (fboundp sym))
  (say "  macro-function     ~S" (and (macro-function sym) t))
  (say "  special-operator-p ~S" (and (special-operator-p sym) t))
  (say "  boundp             ~S" (boundp sym))
  (say "  find-class         ~S" (and (find-class sym nil) t))
  (say "  lambda-list        ~S" (ignore-errors (sb-introspect:function-lambda-list sym)))
  (say "  ftype              ~S" (ftype-of sym))
  (say "  doc(function)      ~S" (documentation sym 'function))
  (say "  doc(variable)      ~S" (documentation sym 'variable))
  (say "  doc(type)          ~S" (documentation sym 'type))
  (say "  source(:function)  ~S" (source-file-of sym :function))
  (say "  source(:macro)     ~S" (source-file-of sym :macro))
  (say "  source(:class)     ~S" (source-file-of sym :class)))

;;; A locally defined function with a declaim, so the ftype is DECLARED.
(declaim (ftype (function (string fixnum) (values list &optional)) probe-declared))
(defun probe-declared (name count)
  "A function with a declared type."
  (declare (ignore name count))
  '())

;;; One with no declaration at all, so any type is DERIVED.
(defun probe-underived (a b)
  "A function with no declared type."
  (+ a b))

(defstruct probe-struct slot-a slot-b)

(defclass probe-class () ((field :initarg :field :accessor probe-class-field)))

(defgeneric probe-generic (x)
  (:documentation "A generic function."))

(probe "declaimed function" 'probe-declared)
(probe "underived function" 'probe-underived)
(probe "CL builtin" 'length)
(probe "CL macro" 'when)
(probe "special operator" 'if)
(probe "CL variable" '*print-pretty*)
(probe "struct accessor" 'probe-struct-slot-a)
(probe "struct constructor" 'make-probe-struct)
(probe "class name" 'probe-class)
(probe "class accessor" 'probe-class-field)
(probe "generic function" 'probe-generic)
(probe "a type name" 'probe-struct)

;;; Two questions the rewrite depends on, asked directly.
(say "~&~%=== can a lambda list be zipped with an ftype's argument list? ===")
(dolist (sym '(probe-declared length probe-underived))
  (let ((ll (ignore-errors (sb-introspect:function-lambda-list sym)))
        (ft (ftype-of sym)))
    (say "  ~S~%    lambda-list ~S~%    ftype       ~S" sym ll ft)
    ;; The ftype is (function (arg-types...) return), so the argument types are
    ;; its second element. Zipping needs them to line up with the lambda list,
    ;; which is exactly what the old positional zip assumed.
    (when (and (consp ft) (eq (first ft) 'function))
      (say "    arg types   ~S" (second ft))
      (say "    return      ~S" (third ft)))))

(format t "~&~A~%" (get-output-stream-string *report*))
