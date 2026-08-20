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
  "Peel every layer of encapsulation off C.

SBCL wraps read errors in an encapsulating condition, and this matters more than
it looks: SB-C:COMPILER-ERROR is **not** a subtype of ERROR -- it is an
ENCAPSULATED-CONDITION, i.e. a plain CONDITION. Code that filters on
(or warning error) therefore drops read errors entirely and reports nothing at
all for a file with a bad package prefix.

There are *two* layers, not one, and an earlier version of this peeled only the
outer one. The chain is

    SB-C:COMPILER-ERROR
      -> SB-C::INPUT-ERROR-IN-COMPILE-FILE   (itself encapsulating)
        -> the real reader condition

Stopping at the middle layer lands on something that is not a SIMPLE-CONDITION,
so classification degraded to :unknown and the message kept SBCL's
\"Stream: #<FORM-TRACKING-STREAM ...>\" trailer. Measured in
docs/experiments/conditions/03-reader-error-api.lisp. The repeat bound is
paranoia about a cycle, not an observed depth."
  #+sbcl
  (loop repeat 10
        while (typep c 'sb-int:encapsulated-condition)
        do (let ((inner (ignore-errors (sb-int:encapsulated-condition c))))
             (if (and inner (not (eq inner c)))
                 (setf c inner)
                 (return))))
  c)

(defun input-error-wrapper (c)
  "The SB-C::INPUT-ERROR-IN-COMPILE-FILE layer of C, if C has one.

Its presence is what identifies a condition as a *read* error rather than a
compile error, and it carries compile-file's own record of where reading
stopped."
  #+sbcl
  (let ((x c))
    (loop repeat 10
          do (cond ((typep x 'sb-c::input-error-in-compile-file) (return x))
                   ((typep x 'sb-int:encapsulated-condition)
                    (let ((inner (ignore-errors (sb-int:encapsulated-condition x))))
                      (if (and inner (not (eq inner x)))
                          (setf x inner)
                          (return nil))))
                   (t (return nil)))))
  #-sbcl (progn c nil))

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
    ("is not of type"                                      :type-conflict        nil)
    ;; Reader errors. Their format arguments are strings rather than symbols --
    ;; the package does not exist, so there is no symbol to have interned -- so
    ;; no index is given and DIAGNOSTIC-SYMBOL stays NIL.
    ("Package ~A does not exist"                           :package-not-found    nil)
    ("unmatched close parenthesis"                         :unmatched-paren      nil)
    ("illegal terminating character"                       :reader-error         nil)
    ("too many colons"                                     :reader-error         nil)
    ("is not external in the ~A package"                   :symbol-not-external  nil))
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

(defun classify (c &key read-error)
  "Return (values kind symbol) for condition C, which must already be unwrapped.

READ-ERROR says C arrived through the reader rather than the compiler, which is
the only thing that distinguishes an unclosed form from an ordinary END-OF-FILE
on a stream. It is not guessed -- the caller knows, because the condition was
wrapped in SB-C::INPUT-ERROR-IN-COMPILE-FILE."
  (when (and read-error (typep c 'end-of-file))
    (return-from classify (values :unclosed-form nil)))
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

(defun condition-message (c kind)
  "The message for C, preferring FORMAT-CONTROL applied to FORMAT-ARGUMENTS over
the printed report.

For most conditions these are the same thing -- applying the control *is* how a
SIMPLE-CONDITION reports itself. The difference shows up on conditions with a
custom report that appends more, and reader errors are exactly that: SBCL's
report tacks on the stream object and a position trailer,

    Package FOO does not exist.

      Line: 2, Column: 37

      Stream: #<SB-INT:FORM-TRACKING-STREAM for \"file /tmp/x.lisp\" {1202B155D3}>

which is noise in an editor and worse noise on a terminal. The position is
carried structurally in this diagnostic already, and the stream identity is of no
interest to anyone.

END-OF-FILE has no format control at all and prints as \"end of file on
#<SB-INT:FORM-TRACKING-STREAM ...>\", which does not say what is actually wrong.
Since KIND already establishes that a form was left open, say that instead."
  (when (eq kind :unclosed-form)
    (return-from condition-message
      "Unexpected end of file: a form opened here is never closed."))
  (or (when (typep c 'simple-condition)
        (let ((control (ignore-errors (simple-condition-format-control c))))
          (when control
            (ignore-errors
             (apply #'format nil control
                    (ignore-errors (simple-condition-format-arguments c)))))))
      (princ-to-string c)))

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

(defun reader-location (wrapper inner)
  "Location for a read error, which has no compiler error context at all.

An earlier draft of the survey recorded that reader errors carry no usable
position and that clef must fall back to tree-sitter for them. That was wrong,
and the printed message gave it away -- \"Line: 2, Column: 32, File-Position: 45\"
is far too precise to be a guess. Measured in
docs/experiments/conditions/03-reader-error-api.lisp, over four failure shapes:

- The INPUT-ERROR-IN-COMPILE-FILE slots POSITION and LINE/COL are filled only
  for end-of-file. For a bad package prefix they are NIL.
- SB-IMPL::STREAM-ERROR-POSITION-INFO is the mirror image: right for a package
  prefix, garbage after end-of-file (it reported line 3, column 512 of a
  two-line file, the stream having run off the end).
- What was correct in *every* case is the FORM-TRACKING-STREAM's record of where
  the current top-level form began: 16, 16, 34, 16 -- and for the stray close
  paren, 34 is the paren itself.

So use the form start. That is also the same unit and the same contract as
COMPILER-ERROR-CONTEXT-FILE-POSITION, which means reader errors and compiler
errors land in one coordinate system instead of two."
  (declare (ignorable wrapper inner))
  #+sbcl
  (when wrapper
    (let* ((stream (or (ignore-errors (stream-error-stream wrapper))
                       (ignore-errors (stream-error-stream inner))))
           (form-start (when stream
                         (ignore-errors
                          (sb-int:form-tracking-stream-form-start-byte-pos stream)))))
      (list :file (when stream (ignore-errors (namestring (pathname stream))))
            :file-position (or form-start
                               (ignore-errors (slot-value wrapper 'sb-c::position)))
            :source-path nil
            :context nil
            :source-form nil)))
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
  (let* ((inner (unwrap c))
         (wrapper (input-error-wrapper c)))
    (multiple-value-bind (kind symbol) (classify inner :read-error (and wrapper t))
      ;; Compiler context first; it is richer. Reader errors have none, and fall
      ;; back to what the reading stream knows.
      (let ((loc (or (context-location (compiler-context))
                     (reader-location wrapper inner))))
        (make-diagnostic
         :severity (condition-severity c)
         :kind kind
         :symbol symbol
         :message (condition-message inner kind)
         :file (getf loc :file)
         :file-position (getf loc :file-position)
         :source-path (getf loc :source-path)
         :context (getf loc :context)
         :source-form (getf loc :source-form)
         :references (ignore-errors (condition-references c)))))))
