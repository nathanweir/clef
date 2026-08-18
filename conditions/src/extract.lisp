(in-package :clef-conditions)

;;;; Turn a Common Lisp condition into structured data.
;;;;
;;;; The point is to stop parsing English. SBCL hands over everything needed:
;;;; the offending symbol arrives through the standard FORMAT-ARGUMENTS slot,
;;;; and the source location through SB-C's compiler error context. See
;;;; docs/surveys/w0-conditions.md for the measurements behind this.
;;;;
;;;; Two consumers: the condition renderer (a humane replacement for SBCL's
;;;; default output) and the language server's diagnostics, which previously
;;;; regex-scraped the printed message and then searched the source text for the
;;;; symbol it recovered.
;;;;
;;;; Prior art: swank/sbcl.lisp reaches for the identical
;;;; (sb-c::find-error-context nil) and has for years -- but it sits inside a
;;;; 2000-line Emacs integration layer and was never factored out. This file is
;;;; that extraction, standing on its own.

(defstruct diagnostic
  "One problem, located and classified.

SEVERITY is :error, :warning, :style-warning or :note.

KIND is a keyword naming the shape of the problem -- :undefined-function,
:undefined-variable, :unused-variable, :wrong-argument-count, :type-conflict --
or :unknown when the condition is not one we classify. An :unknown diagnostic is
still fully usable: it carries the message and the location.

FILE-POSITION is a byte offset into FILE, which is the unit clef's language
server already works in."
  (severity :error)
  (kind :unknown)
  (symbol nil)
  (message "" :type string)
  (file nil)
  (file-position nil)
  (source-path nil)
  (context nil)
  (source-form nil)
  (references nil))

(defun unwrap (c)
  "SBCL wraps read and compile-time errors in an encapsulating condition.

This matters more than it looks: SB-C:COMPILER-ERROR is **not** a subtype of
ERROR -- it is an ENCAPSULATED-CONDITION, i.e. a plain CONDITION. Code that
filters on (or warning error) therefore drops read errors entirely and reports
nothing at all for a file with a bad package prefix. Unwrapping recovers the real
condition underneath, which is an ERROR and classifies properly."
  #+sbcl (if (typep c 'sb-int:encapsulated-condition)
             (or (ignore-errors (sb-int:encapsulated-condition c)) c)
             c)
  #-sbcl c)

(defun condition-severity (c)
  "Classify C by condition type alone. Type is authoritative here; nothing is
being guessed from text."
  (typecase (unwrap c)
    (style-warning :style-warning)
    (warning :warning)
    (error :error)
    (t :note)))

;;; ---------------------------------------------------------------------------
;;; Classification
;;;
;;; Honest note on method: the offending SYMBOL comes out of FORMAT-ARGUMENTS
;;; and is genuine structured data. The KIND still requires recognising which
;;; message we are looking at, because SBCL signals most of these as plain
;;; SIMPLE-WARNING / SIMPLE-STYLE-WARNING rather than as distinct classes.
;;;
;;; We match on the FORMAT-CONTROL -- the template -- not on the rendered
;;; output. That is much steadier: no interpolated values, no line wrapping, no
;;; pretty-printer decisions. And an unrecognised template degrades to :unknown
;;; while keeping message and location, rather than losing the diagnostic.
;;; ---------------------------------------------------------------------------

(defun format-control-string (c)
  "FORMAT-CONTROL may be a string or an SB-FORMAT::FMT-CONTROL object. Render
either to a string for matching."
  (when (typep c 'simple-condition)
    (let ((control (ignore-errors (simple-condition-format-control c))))
      (typecase control
        (string control)
        (null nil)
        (t (princ-to-string control))))))

(defun condition-format-arguments (c)
  (when (typep c 'simple-condition)
    (ignore-errors (simple-condition-format-arguments c))))

(defparameter *templates*
  ;; (substring-of-format-control  kind  index-of-symbol-in-format-arguments)
  ;;
  ;; The index is where the interesting symbol sits in FORMAT-ARGUMENTS:
  ;;   "undefined function: FOO"           -> (FUNCTION FOO)      index 1
  ;;   "The variable X is ... never used"  -> (X)                 index 0
  ;;   "The function F is called with ..." -> (F NIL 3 0)         index 0
  '(("undefined ~(~A~)"                                    :undefined            1)
    ("is defined but never used"                           :unused-variable      0)
    ("is called~@[ by ~S~] with ~R argument"                :wrong-argument-count 0)
    ("conflicts with its asserted type"                    :type-conflict        nil)
    ("conflicting with the declared function return type"  :type-conflict        nil)
    ("conflicting with its asserted type"                  :type-conflict        nil)
    ("is not of type"                                      :type-conflict        nil))
  "Recognised FORMAT-CONTROL templates, most specific first.")

(defun resolve-kind (kind args)
  "SBCL uses one template for undefined functions, variables and types,
distinguishing them by the first format argument. Split them back apart.

Compared by symbol NAME rather than identity: SBCL passes the keyword :FUNCTION
here, but the package of that marker is an implementation detail and matching on
identity broke silently when it turned out not to be CL:FUNCTION."
  (if (eq kind :undefined)
      (let ((what (first args)))
        (if (symbolp what)
            (let ((name (symbol-name what)))
              (cond ((string= name "FUNCTION") :undefined-function)
                    ((string= name "VARIABLE") :undefined-variable)
                    ((string= name "TYPE") :undefined-type)
                    (t :undefined)))
            :undefined))
      kind))

(defun classify (c)
  "Return (values kind symbol) for condition C."
  (let ((control (format-control-string c))
        (args (condition-format-arguments c)))
    (if (null control)
        (values :unknown nil)
        (loop for (needle kind index) in *templates*
              when (search needle control)
                return (let ((resolved (resolve-kind kind args))
                             (sym (when (and index (< index (length args)))
                                    (nth index args))))
                         (values resolved (when (symbolp sym) sym)))
              finally (return (values :unknown nil))))))

;;; ---------------------------------------------------------------------------
;;; Source location
;;;
;;; SB-C internals, same standing as the arena work: pinned SBCL, and every
;;; access degrades to NIL rather than erroring, so an upgrade that moves these
;;; accessors costs location, not correctness.
;;; ---------------------------------------------------------------------------

(defun compiler-context ()
  "SBCL's compiler error context for the condition currently being signalled, or
NIL outside compilation."
  #+sbcl (ignore-errors (sb-c::find-error-context nil))
  #-sbcl nil)

(defun context-location (ctx)
  "Location fields of CTX as a plist, or NIL."
  (declare (ignorable ctx))
  #+sbcl
  (when ctx
    (list :file (ignore-errors (sb-c::compiler-error-context-file-name ctx))
          :file-position (ignore-errors (sb-c::compiler-error-context-file-position ctx))
          :source-path (ignore-errors (sb-c::compiler-error-context-original-source-path ctx))
          :context (ignore-errors (sb-c::compiler-error-context-context ctx))
          :source-form (ignore-errors (sb-c::compiler-error-context-original-source ctx))))
  #-sbcl nil)

(defun condition-references (c)
  "SBCL attaches manual references to some conditions -- the source of the
\"See also: The SBCL Manual, Node ...\" trailer. Looked up by slot name rather
than by a hardcoded internal symbol, so it simply returns NIL if the slot moves."
  (let* ((slot-name
           (loop for slot in (sb-mop:class-slots (class-of c))
                 for name = (sb-mop:slot-definition-name slot)
                 when (string= (symbol-name name) "REFERENCES")
                   return name)))
    (when (and slot-name (slot-boundp c slot-name))
      (slot-value c slot-name))))

;;; ---------------------------------------------------------------------------
;;; Entry point
;;; ---------------------------------------------------------------------------

(defun extract (c)
  "Build a DIAGNOSTIC from condition C.

Must be called from inside a HANDLER-BIND while the condition is being
signalled. The compiler error context is dynamic state, not carried on the
condition object, so it is gone once the handler returns -- which is precisely
why this is awkward to expose as a plain condition-to-string function, and
plausibly part of why nobody has."
  (multiple-value-bind (kind symbol) (classify (unwrap c))
    (let ((loc (context-location (compiler-context))))
      (make-diagnostic
       :severity (condition-severity c)
       :kind kind
       :symbol symbol
       :message (princ-to-string c)
       :file (getf loc :file)
       :file-position (getf loc :file-position)
       :source-path (getf loc :source-path)
       :context (getf loc :context)
       :source-form (getf loc :source-form)
       :references (ignore-errors (condition-references c))))))
