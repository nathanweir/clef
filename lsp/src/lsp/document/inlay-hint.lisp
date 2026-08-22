(in-package :clef-lsp/document)

;;;; textDocument/inlayHint -- parameter names at call sites.
;;;;
;;;; Common Lisp leans hard on positional arguments, and the reader has to
;;;; remember which is which:
;;;;
;;;;     (subseq line 4 12)          becomes    (subseq line start: 4 end: 12)
;;;;     (make-point 0.0d0 1.0d0)               (make-point x: 0.0d0 y: 1.0d0)
;;;;
;;;; The lambda list is already known, so this is nearly free, and it is the
;;;; single most useful hint kind for a language with no named arguments outside
;;;; &KEY.
;;;;
;;;; Quiet on purpose. A hint on every argument of every call is unreadable, so
;;;; hints are suppressed where they add nothing: when the argument is a variable
;;;; already spelled like the parameter, when the operator is a macro or special
;;;; operator (whose "arguments" are not parameters in any useful sense), and
;;;; past the first lambda-list marker, since &OPTIONAL and &KEY arguments do not
;;;; line up positionally.

(defparameter +inlay-hint-kind-parameter+ 2
  "InlayHintKind.Parameter, from the spec's enumeration.")

(defun hintable-operator-p (sym)
  "Is SYM a function whose parameters are worth naming?

Macros and special operators are excluded. (LET ((x 1)) ...) does not have a
parameter called BINDINGS in any sense a reader benefits from, and labelling it
would be noise on the most common forms in the language."
  (and sym
       (fboundp sym)
       (not (macro-function sym))
       (not (special-operator-p sym))))

(defun indexed-lambda-list (name)
  "Parameter names for NAME read from its source, or NIL.

The image only knows functions that have been loaded, which excludes the one the
user just wrote -- and a hint on the call you are in the middle of writing is
worth more than one on SUBSEQ. clef has the defining form indexed, and the
lambda list is right there in it.

Only consults documents the client has open, so this costs no file I/O. The
common case it is meant for -- calling something defined in the file you are
editing -- is covered by that."
  (let ((definition (first (clef-symbols:lookup-in-workspace-index name))))
    (when definition
      (let* ((location (clef-symbols:symbol-definition-location definition))
             (file-path (when location (clef-symbols:location-file-path location)))
             (source (when file-path
                       (gethash (clef-util:path-to-file-uri file-path) ctx:documents)))
             (form (clef-symbols:symbol-definition-form-node definition)))
        (when (and source form)
          ;; FORM-NODE for a DEFUN is already the :DEFUN node -- CHECK-FOR-DEFUN
          ;; fires on that node and records it. Looking for a :DEFUN child of it
          ;; finds nothing, which is why the first version of this silently
          ;; produced no hints for user code at all.
          (let* ((defun-node (if (eq (clef-symbols:node-kind-of form) :defun)
                                 form
                                 (find :defun (ts:node-children form)
                                       :key #'clef-symbols:node-kind-of)))
                 (header (when defun-node
                           (find :defun-header (ts:node-children defun-node)
                                 :key #'clef-symbols:node-kind-of)))
                 (lambda-list (when header
                                (find :list-lit (ts:node-children header)
                                      :key #'clef-symbols:node-kind-of))))
            (when lambda-list
              (trim-lambda-list
               (loop for child in (ts:node-children lambda-list)
                     when (eq (clef-symbols:node-kind-of child) :sym-lit)
                       collect (let ((text (ignore-errors
                                            (clef-parser/parser:node-text child source))))
                                 (when text (intern (string-upcase text) :keyword))))))))))))

(defun required-parameter-names (sym name)
  "The names of the required parameters for a call to NAME.

The image first, since it is authoritative and knows the whole standard library;
clef's own index second, for code the image has never seen."
  (or (when sym
        (trim-lambda-list
         (or (ignore-errors (sb-introspect:function-lambda-list sym)) '())))
      (indexed-lambda-list name)))

(defun hint-worth-showing-p (parameter-name argument-node source)
  "Would labelling this argument tell the reader anything?

An argument already spelled like its parameter -- (make-point x y) -- gains
nothing from being told so, and the clutter costs more than the information."
  (let ((text (ignore-errors (clef-parser/parser:node-text argument-node source))))
    (not (and text
              (string-equal (string-trim "'" text)
                            (princ-to-string parameter-name))))))

(defun call-inlay-hints (node source package-designator)
  "Hints for one call form, or NIL if it is not a call worth hinting."
  (let ((children (remove :comment (ts:node-children node)
                          :key #'clef-symbols:node-kind-of)))
    (when (and children
               (eq (clef-symbols:node-kind-of (first children)) :sym-lit))
      (let* ((operator-text (ignore-errors
                             (clef-parser/parser:node-text (first children) source)))
             (sym (when operator-text (lookup-symbol operator-text package-designator))))
        ;; Hintable when the image says it is a plain function, OR when the image
        ;; has never heard of it and clef's index has -- which is the case for
        ;; the code being written right now.
        (when (and operator-text
                   (or (hintable-operator-p sym)
                       (and (not sym) t)
                       (and sym (not (fboundp sym)))))
          (loop for argument in (rest children)
                for name in (required-parameter-names
                             (when (hintable-operator-p sym) sym)
                             operator-text)
                when (hint-worth-showing-p name argument source)
                  collect (dict "position"
                                (make-position
                                 (clef-parser/parser:node-start-point-row argument)
                                 (clef-parser/parser:node-start-point-column argument))
                                "label" (format nil "~A:" (string-downcase
                                                           (princ-to-string name)))
                                "kind" +inlay-hint-kind-parameter+
                                ;; Space after the label, none before: the hint
                                ;; sits immediately left of the argument.
                                "paddingRight" t)))))))

(defun position-within-range-p (line character range)
  "Is (LINE, CHARACTER) inside RANGE, which is the client's requested window?"
  (let* ((start (href range "start"))
         (end (href range "end"))
         (start-line (href start "line"))
         (end-line (href end "line")))
    (and (>= line start-line) (<= line end-line))))

(defun handle-text-document-inlay-hint (message)
  "Handle a textDocument/inlayHint request.

The client asks for a range -- the visible window -- rather than the whole file,
and honouring that matters: hints are recomputed on every scroll."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (range (href params "range"))
         (text (gethash document-uri ctx:documents)))
    (slog :debug "[textDocument/inlayHint] Document: ~A" document-uri)
    (if (null text)
        #()
        (let* ((tree (clef-parser/parser:parse-string text))
               (package-designator
                 (let ((pkg (clef-parser/utils:find-package-declaration tree text)))
                   (when pkg (package-name pkg))))
               (hints '()))
          (labels ((walk (node)
                     (when node
                       (when (member (clef-symbols:node-kind-of node) '(:list-lit :vec-lit))
                         (dolist (hint (call-inlay-hints node text package-designator))
                           (let ((position (gethash "position" hint)))
                             (when (or (null range)
                                       (position-within-range-p
                                        (gethash "line" position)
                                        (gethash "character" position)
                                        range))
                               (push hint hints)))))
                       (dolist (child (ts:node-children node))
                         (walk child)))))
            (walk tree))
          (slog :debug "[textDocument/inlayHint] ~A hint(s)" (length hints))
          (coerce (nreverse hints) 'vector)))))
