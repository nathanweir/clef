(in-package :clef-lsp/document)

;;;; textDocument/hover.
;;;;
;;;; This used to call (describe sym stream) and recover everything it needed
;;;; with five regexes over SBCL's English prose -- "(\\S+) names a compiled
;;;; function", "Lambda-list:\\s+\\((.*?)\\)", and so on. That is exactly the
;;;; anti-pattern W0 removed from diagnostics: DESCRIBE formats for humans and
;;;; its output is not an interface.
;;;;
;;;; Every field has a structured equivalent, and the old file already reached
;;;; for two of them while routing the main path through prose:
;;;;
;;;;   lambda list   SB-INTROSPECT:FUNCTION-LAMBDA-LIST
;;;;   type          (SB-INT:INFO :FUNCTION :TYPE sym), read with TYPE-SPECIFIER
;;;;   docstring     DOCUMENTATION -- standard Common Lisp
;;;;   source file   SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME
;;;;   what it is    FBOUNDP / MACRO-FUNCTION / SPECIAL-OPERATOR-P / FIND-CLASS
;;;;
;;;; Measured in docs/experiments/lsp/04-hover-sources.lisp against every shape
;;;; hover meets: declaimed and underived functions, builtins, macros, special
;;;; operators, variables, classes, structs, generics and accessors.
;;;;
;;;; The presentation is kept. Showing declared types at the point of use is a
;;;; direct answer to motivation.md §7's typing thread, and this is the one place
;;;; clef consumes SBCL's type knowledge -- which makes it the natural surface
;;;; for W4. It was the way it was populated that was wrong, not the idea.

;; Returns NIL when the position is not on a symbol -- whitespace, a comment, a
;; paren -- which is the common case, not an edge case. The return type was
;; declared as STRING, so SBCL's safety check turned every such hover into
;; "Internal server error: The value NIL is not of type STRING".
;;
;; Worth noting as evidence for the typing workstream (roadmap W4): the declaim
;; was wrong, and SBCL enforced it rather than letting the lie stand. The fix is
;; to state the truth, not to remove the declaration.
(declaim (ftype (function (string integer integer) (or string null))
                find-symbol-at-position))
(defun find-symbol-at-position (document-text line char)
       "Finds the symbol within some text at a given position"
       (let* ((tree (clef-parser/parser:parse-string document-text))
              (result nil))
             (labels ((position-in-range-p (node)
                                           (multiple-value-bind (start-line start-char end-line end-char)
                                                                (clef-parser/parser:node-range node)
                                                                (or
                                                                  ;; Same line, char in range
                                                                  (and (= line start-line) (= line end-line)
                                                                       (<= start-char char) (< char end-char))
                                                                  ;; Multi-line node
                                                                  (and (or (> line start-line)
                                                                           (and (= line start-line) (>= char start-char)))
                                                                       (or (< line end-line)
                                                                           (and (= line end-line) (< char end-char)))))))
                      (visit-node (node)
                                  (when (and (and (consp (ts:node-type node)))
                                             (eq (second (ts:node-type node)) :SYM-LIT)
                                             (position-in-range-p node))
                                        (setf result node)
                                        (return-from visit-node))
                                  (dolist (child (ts:node-children node))
                                          (when (not result) (visit-node child)))))
                     (when tree (visit-node tree))
                     (when result
                           (clef-parser/parser:node-text result document-text)))))

(defun lookup-symbol (name &optional pkg-name)
       "The symbol NAME names, in PKG-NAME or the current package."
       (let ((pkg (if pkg-name
                      (find-package (string-upcase pkg-name))
                      *package*))
             (sym-name (if (and name
                                (plusp (length name))
                                (char= #\| (char name 0))
                                (char= #\| (char name (1- (length name)))))
                           (subseq name 1 (- (length name) 1))
                           name)))
            (when (and sym-name pkg)
                  (or (find-symbol sym-name pkg)
                      (find-symbol (string-upcase sym-name) pkg)))))

;;; ---------------------------------------------------------------------------
;;; What a symbol is, and what is known about it
;;; ---------------------------------------------------------------------------

(defun symbol-role (sym)
       "What SYM is, as far as hover cares. NIL if it is nothing in particular.

Order matters: a macro is FBOUNDP too, and a class name may also be a variable."
       (cond ((special-operator-p sym) :special-operator)
             ((macro-function sym) :macro)
             ((and (fboundp sym)
                   (typep (fdefinition sym) 'generic-function)) :generic)
             ((fboundp sym) :function)
             ((find-class sym nil) :class)
             ((boundp sym) :variable)
             (t nil)))

(defun role-operator (role)
       "The defining form a reader would recognise for ROLE."
       (ecase role
         (:special-operator "special operator")
         (:macro "defmacro")
         (:generic "defgeneric")
         (:function "defun")
         (:class "defclass")
         (:variable "defvar")))

(defun function-type-of (sym)
       "SYM's function type as a readable specifier, or NIL.

This is the declared type when there is a DECLAIM, and SBCL's derived type
otherwise -- which is why hovering a function nobody annotated still reports a
useful return type."
       (ignore-errors
        (let ((ctype (sb-int:info :function :type sym)))
             (when ctype (sb-kernel:type-specifier ctype)))))

(defun ftype-argument-types (ftype)
       (when (and (consp ftype) (eq (first ftype) 'function) (listp (second ftype)))
             (second ftype)))

(defun ftype-return-type (ftype)
       "The return type, unwrapped.

SBCL always spells a single return value as (VALUES X &OPTIONAL), which is
accurate and unreadable. * means nothing is known and is reported as nothing."
       (let ((ret (when (and (consp ftype) (eq (first ftype) 'function))
                        (third ftype))))
            (cond ((null ret) nil)
                  ((eq ret '*) nil)
                  ((and (consp ret) (eq (first ret) 'values))
                   (let ((values (remove '&optional (rest ret))))
                        (cond ((null values) nil)
                              ((= 1 (length values)) (first values))
                              (t (cons 'values values)))))
                  (t ret))))

(defun annotated-parameters (lambda-list argument-types)
       "Pair each parameter name with its type, or NIL when nothing is known.

Markers -- &optional, &rest, &key -- appear in BOTH lists, and are skipped in
both, which is what keeps the pairing aligned. The old implementation split two
strings on spaces and zipped them positionally, so it misaligned the moment a
lambda list contained a marker or a default: (x &optional (y 5)) splits into four
tokens against a two-element type list, and every annotation after the first was
wrong."
       (let ((types argument-types)
             (result '()))
            ;; NORMALIZE first: a dotted lambda list such as
            ;; DEFINE-METHOD-COMBINATION's (NAME . ARGS) makes DOLIST signal,
            ;; and hover then answered "Internal server error" for that symbol.
            (dolist (item (normalize-lambda-list lambda-list) (nreverse result))
                    (cond
                      ((lambda-list-marker-p item)
                       (when (and types (lambda-list-marker-p (first types)))
                             (pop types))
                       (push (list item nil) result))
                      (t
                        (let ((name (if (consp item) (first item) item))
                              (type (unless (and types (lambda-list-marker-p (first types)))
                                            (pop types))))
                             (push (list name type) result)))))))

(defun informative-type-p (type)
       "Is TYPE worth showing?

T means \"anything\", which is what an unannotated parameter reports. Printing
`;; T` beside every parameter looks like an annotation and carries nothing."
       (and type (not (eq type t)) (not (eq type '*))))

(defun source-file-of (sym role)
       "The file SYM was defined in, or NIL.

The kind has to match: a macro is not found under :FUNCTION."
       (let ((kind (case role
                     (:macro :macro)
                     (:class :class)
                     ((:function :generic) :function)
                     (:variable :variable)
                     (t nil))))
            (when kind
                  (ignore-errors
                   (let ((sources (sb-introspect:find-definition-sources-by-name sym kind)))
                        (when sources
                              (let ((path (sb-introspect:definition-source-pathname
                                            (first sources))))
                                   (when path (namestring path)))))))))

;;; ---------------------------------------------------------------------------
;;; Rendering
;;; ---------------------------------------------------------------------------

(defun render-signature (role sym parameters)
       "The defining form as a reader would write it."
       (format nil "(~A ~A~@[ (~{~A~^ ~})~])"
               (role-operator role)
               (string-downcase (symbol-name sym))
               (when parameters
                     (mapcar (lambda (entry) (string-downcase (princ-to-string (first entry))))
                             parameters))))

(defun render-types (parameters return-type)
       "A type block, or NIL when nothing is known.

Omitted entirely rather than filled with T, so its presence means there is
something to read."
       (let ((interesting (remove-if-not (lambda (entry) (informative-type-p (second entry)))
                                         parameters)))
            (when (or interesting (informative-type-p return-type))
                  (with-output-to-string (out)
                    (dolist (entry interesting)
                            (format out "~A : ~A~%"
                                    (string-downcase (princ-to-string (first entry)))
                                    (second entry)))
                    (when (informative-type-p return-type)
                          (format out "=> ~A~%" return-type))))))

(defun hover-markdown (sym)
       "Markdown for SYM, or NIL if there is nothing to say."
       (let ((role (symbol-role sym)))
            (when role
                  (let* ((ftype (when (member role '(:function :generic)) (function-type-of sym)))
                         (lambda-list (ignore-errors (sb-introspect:function-lambda-list sym)))
                         (parameters (when (member role '(:function :generic :macro))
                                           (annotated-parameters lambda-list
                                                                 (ftype-argument-types ftype))))
                         (return-type (ftype-return-type ftype))
                         (types (render-types parameters return-type))
                         (docstring (or (documentation sym 'function)
                                        (documentation sym 'variable)
                                        (documentation sym 'type)))
                         (source (source-file-of sym role))
                         (package (package-name (symbol-package sym))))
                        (with-output-to-string (out)
                          (format out "```lisp~%~A~%" (render-signature role sym parameters))
                          (when types (format out "~%~A" types))
                          (format out "```~%")
                          (when docstring (format out "~%~A~%" docstring))
                          (format out "~%---~%~%`~A:~A`~%"
                                  package (symbol-name sym))
                          (when source (format out "~%*~A*~%" source)))))))

;;; ---------------------------------------------------------------------------
;;; Hover for code the image has never seen
;;;
;;; Everything in HOVER-MARKDOWN asks the running image: FBOUNDP, DOCUMENTATION,
;;; the ftype. That is fine for Common Lisp itself and for libraries the
;;; workspace has loaded -- and it is precisely the code being written *right
;;; now* that the image has never seen, so the richest hover went to the symbols
;;; that needed it least.
;;;
;;; This used to render as `(defun repair-source)` plus an apology, while the
;;; lambda list and docstring it apologised for sat in the very form-node the
;;; indexer had stored. They are source text, not image state -- so read them
;;; from the source. What genuinely cannot be had without the image is derived
;;; TYPE information, and that is now all this path lacks.
;;; ---------------------------------------------------------------------------

(defun definition-source-text (def)
       "The text of the file DEF was defined in, or NIL.

The open buffer wins over the disk copy: the form-node's coordinates describe
the text the indexer last saw, which for an open document is the buffer."
       (let* ((location (clef-symbols:symbol-definition-location def))
              (file (when location (clef-symbols:location-file-path location))))
             (when file
                   (or (let ((uri (clef-util:path-to-file-uri file)))
                            (when uri (gethash uri ctx:documents)))
                       (ignore-errors (uiop:read-file-string file))))))

(defun string-literal-value (text)
       "The value of a string literal's source TEXT: quotes stripped, escapes undone."
       (when (and text (>= (length text) 2) (char= (char text 0) #\"))
             (with-output-to-string (out)
               (loop with i = 1 and n = (1- (length text))
                     while (< i n)
                     do (let ((c (char text i)))
                             (cond ((and (char= c #\\) (< (1+ i) n))
                                    (write-char (char text (1+ i)) out)
                                    (incf i 2))
                                   (t (write-char c out) (incf i))))))))

(defparameter +lambda-list-kinds+ '(:function :macro :method)
  "Definition kinds whose element 2 is a lambda list.

The gate matters: element 2 of a DEFVAR is its value and element 2 of a DEFCLASS
is its superclass list, and either would render as a convincing-looking wrong
signature.")

(defun indexed-signature-parts (def source)
       "(values lambda-list-text docstring) read from DEF's defining form, either
or both NIL.

FORM-ELEMENTS -- the same flattening the diagnostics path uses to mirror how
SBCL indexes a form -- turns the stored form-node into (head name . rest).
The docstring rules follow the language's:

  - after a lambda list, a string is the docstring only when more body follows;
    (defun f () \"x\") RETURNS the string.
  - a DEFVAR docstring is element 3, and there it may legally be last.
  - otherwise a string at element 2 with anything after it is doc-like -- which
    is what picks up DEFTEST-style project macros."
       (let ((form (clef-symbols:symbol-definition-form-node def)))
            (when (and form source)
                  (let* ((elements (form-elements form))
                         (name-node (nth 1 elements))
                         (kind (clef-symbols:symbol-definition-kind def))
                         (third-el (nth 2 elements)))
                        ;; The stored form must actually be NAME's definition.
                        ;; A defstruct's accessors all store the whole DEFSTRUCT
                        ;; as their form-node, where element 1 names the struct,
                        ;; not the accessor -- extracting would caption MAKE-FOO
                        ;; with FOO's slot list.
                        (when (and name-node
                                   (string-equal
                                     (string-upcase
                                       (clef-symbols:symbol-definition-symbol-name def))
                                     (string-upcase
                                       (or (ignore-errors
                                            (clef-parser/parser:node-text name-node source))
                                           ""))))
                              (let* ((lambda-node
                                       (when (and (member kind +lambda-list-kinds+)
                                                  third-el
                                                  (eq (node-kind third-el) :list-lit))
                                             third-el))
                                     ;; A DEFVAR's element 2 is its VALUE; the
                                     ;; docstring, when present, is element 3 --
                                     ;; same slot as after a lambda list.
                                     (doc-index (if (or lambda-node
                                                        (member kind '(:variable :constant)))
                                                    3
                                                    2))
                                     (doc-node (nth doc-index elements))
                                     (docstring
                                       (when (and doc-node
                                                  (eq (node-kind doc-node) :str-lit)
                                                  ;; Legally last only for DEFVAR.
                                                  (or (nth (1+ doc-index) elements)
                                                      (member kind '(:variable :constant))))
                                             (string-literal-value
                                               (ignore-errors
                                                (clef-parser/parser:node-text doc-node source))))))
                                   (values (when lambda-node
                                                 (ignore-errors
                                                  (clef-parser/parser:node-text lambda-node source)))
                                           docstring)))))))

(defun indexed-hover-markdown (name)
       "Markdown from clef's own index, for a symbol the image does not have."
       (let ((defs (clef-symbols:lookup-in-workspace-index name)))
            (when defs
                  (let* ((def (first defs))
                         (location (clef-symbols:symbol-definition-location def))
                         (file (when location (clef-symbols:location-file-path location)))
                         (kind (clef-symbols:symbol-definition-kind def))
                         (package (clef-symbols:symbol-definition-package-name def))
                         (source (definition-source-text def)))
                        (multiple-value-bind (lambda-list docstring)
                                             (indexed-signature-parts def source)
                          (with-output-to-string (out)
                            (format out "```lisp~%(~A ~A~@[ ~A~])~%```~%"
                                    (case kind
                                      (:function "defun") (:macro "defmacro")
                                      (:method "defmethod") (:class "defclass")
                                      (:struct "defstruct") (:type "deftype")
                                      (:variable "defvar") (:constant "defconstant")
                                      (:package "defpackage")
                                      (t "definition"))
                                    (string-downcase name)
                                    lambda-list)
                            (when docstring (format out "~%~A~%" docstring))
                            (format out "~%---~%~%`~@[~A:~]~A` ~A~%"
                                    (when package (string-upcase (princ-to-string package)))
                                    (string-upcase name)
                                    ;; Say what is genuinely missing, not more.
                                    "-- from source; not loaded, so no derived types")
                            (when file (format out "~%*~A*~%" file))))))))

(defun handle-text-document-hover (message)
       "Handle a textDocument/hover request."
       (let* ((params (clef-jsonrpc/types:request-params message))
              (document-uri (href params "text-document" "uri"))
              (hover-line (href params "position" "line"))
              (hover-char (href params "position" "character"))
              (document-text (gethash document-uri ctx:documents)))
             (if (null document-text)
                 (dict "contents" #())
                 (let* ((symbol-at-pos (find-symbol-at-position document-text
                                                                hover-line hover-char))
                        (tree (clef-parser/parser:parse-string document-text))
                        (symbol-pkg (or (clef-parser/utils:find-package-declaration
                                          tree document-text)
                                        *package*))
                        (sym (when symbol-at-pos
                                   (lookup-symbol symbol-at-pos (package-name symbol-pkg))))
                        (markdown (or (when sym (hover-markdown sym))
                                      (when symbol-at-pos
                                            (indexed-hover-markdown symbol-at-pos)))))
                       (dict "contents" (or markdown #()))))))
