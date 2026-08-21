;;;; Drive every LSP operation against representative Common Lisp and record
;;;; what comes back.
;;;;
;;;; This is the W1 review's first pass, and it follows the conclusion of
;;;; surveys/clef-state.md §9: *survey by execution, not by reading*. Every
;;;; defect found in that survey came from running the project. The first four
;;;; probes of this review, run through Claude Code's own LSP client, found three
;;;; bugs that reading would plausibly have missed.
;;;;
;;;; Claude Code's LSP tool only exposes nine operations, so it cannot reach
;;;; completion, signatureHelp, formatting or documentHighlight. This drives the
;;;; handlers directly instead, via the test framework's CALL-HANDLER.
;;;;
;;;; Not a test. It asserts nothing; it prints what happened so a human can see
;;;; what is wrong. Findings get promoted into surveys/lsp-review.md, and the
;;;; ones we fix become real tests.
;;;;
;;;; Run: sbcl --noinform --non-interactive --load docs/experiments/lsp/01-operation-sweep.lisp

;;; Loading mirrors lsp/test/run-tests.lisp, minus the part that runs the suite.
;;; INIT-SERVER lives in document-tests.lisp, so that file has to come along.

#-quicklisp
(let ((init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file init) (load init)))

(setf *compile-verbose* nil *compile-print* nil
      *load-verbose* nil *load-print* nil)

(defparameter *repo-root* (truename "."))
(defparameter *lsp-root* (merge-pathnames "lsp/" *repo-root*))

(asdf:initialize-output-translations
 `(:output-translations
   ((,*repo-root* :**/ :*.*.*) (,*repo-root* "build" :**/ :*.*.*))
   :inherit-configuration))

(require 'sb-posix)
(require 'sb-introspect)

(handler-bind ((warning #'muffle-warning))
  (asdf:load-asd (merge-pathnames "conditions/clef-conditions.asd" *repo-root*))
  (asdf:load-asd (merge-pathnames "clef-lsp.asd" *lsp-root*))
  (asdf:load-system :clef-lsp))

(ql:quickload '(:serapeum :bordeaux-threads :com.inuoe.jzon :babel :cl-ppcre) :silent t)
(setf clef-log:*log-mode* :none)

(handler-bind ((warning #'muffle-warning))
  (dolist (f '("test/package.lisp" "test/framework.lisp" "test/document-tests.lisp"))
    (load (merge-pathnames f *lsp-root*))))

(in-package :clef-test)

;;; ---------------------------------------------------------------------------
;;; The specimen.
;;;
;;; Deliberately covers what idiomatic CL actually contains, not what is easy:
;;; every major definition form, the three binding constructs, shadowing, a
;;; package-qualified reference, and the two places a symbol's NAME appears
;;; without being a reference at all (a string and a comment).
;;; ---------------------------------------------------------------------------

(defparameter *specimen* "(defpackage :sweep-demo (:use :cl))
(in-package :sweep-demo)

(defvar *counter* 0)
(defconstant +limit+ 100)

(deftype small-int () '(integer 0 100))

(defstruct point x y)

(defclass shape ()
  ((name :initarg :name :accessor shape-name)
   (area :initarg :area :accessor shape-area)))

(define-condition shape-error (error)
  ((shape :initarg :shape :accessor shape-error-shape)))

(defgeneric describe-shape (s))

(defmethod describe-shape ((s shape))
  (shape-name s))

(defmacro with-counter ((var) &body body)
  `(let ((,var *counter*)) ,@body))

(defun make-origin ()
  (make-point :x 0 :y 0))

(defun uses-everything (radius)
  (let ((area (* radius radius))
        (name \"area\"))            ; the STRING \"area\" is not a reference
    ;; the word area in a comment is not a reference either
    (flet ((scale (area) (* area 2)))
      (labels ((twice (n) (scale (scale n))))
        (list area name (twice area) (shape-area (make-instance 'shape)))))))

(defun calls-across-file ()
  (make-origin))

(defun calls-the-generic (s)
  (describe-shape s))

(defun calls-the-macro ()
  (with-counter (c) c))
")

;;; A real file on disk, not a made-up URI.
;;;
;;; The first version of this probe used a synthetic file:///sweep/... URI and
;;; reported that *every* definition form failed to resolve -- including DEFUN,
;;; which the live server resolves perfectly well. That was the harness, not the
;;; server: symbol resolution needs a file that exists, and the symbol map is
;;; built by didChange rather than didOpen (a finding in its own right, see
;;; surveys/lsp-review.md). Reporting those as defects would have been the third
;;; bad-probe result of the day.
(defparameter *specimen-path*
  (let ((path (merge-pathnames "tmp/test/sweep-specimen.lisp" (truename "."))))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string *specimen* s))
    (namestring path)))

(defparameter *uri* (format nil "file://~A" *specimen-path*))

;;; ---------------------------------------------------------------------------
;;; Reporting
;;; ---------------------------------------------------------------------------

(defparameter *findings* '())

(defun note (severity label detail)
  (push (list severity label detail) *findings*)
  (format t "    ~A ~A~@[ -- ~A~]~%"
          (ecase severity (:bug "BUG ") (:gap "GAP ") (:ok "ok  ") (:info "    "))
          label detail))

(defun line-of (text needle)
  "0-based line index of the line containing NEEDLE, for readable probes."
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (loop for l in lines for i from 0
          when (search needle l) return i)))

(defun col-of (text needle sub)
  "0-based column of SUB within the line containing NEEDLE."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (line (find-if (lambda (l) (search needle l)) lines)))
    (when line (search sub line))))

(defun open-specimen (call)
  ;; didChange as well as didOpen, because didOpen does not build the symbol
  ;; map -- it only stores the text. Every existing test does this dance too.
  (funcall call "textDocument/didOpen"
           (dict "textDocument" (dict "uri" *uri* "languageId" "lisp"
                                      "version" 1 "text" *specimen*))
           nil)
  (funcall call "textDocument/didChange"
           (dict "textDocument" (dict "uri" *uri* "version" 2)
                 "contentChanges" (vector (dict "text" *specimen*)))
           nil))

(defun result-of (response)
  (response-result-safe response))

(defun count-items (result)
  (cond ((null result) 0)
        ((vectorp result) (length result))
        ((listp result) (length result))
        (t 1)))

;;; ---------------------------------------------------------------------------
;;; Probes
;;; ---------------------------------------------------------------------------

(defun probe-definition-forms (call)
  (format t "~&~%== go-to-definition, by definition form ==~%")
  ;; Each entry: what we search for, the token to land on, and where it is
  ;; defined. A form that does not resolve is a hole in the symbol index.
  (dolist (spec '(("(make-origin)" "make-origin" "defun")
                  ("(with-counter (c) c)" "with-counter" "defmacro")
                  ("*counter*)) ,@body" "*counter*" "defvar")
                  ("(let ((area (* radius radius" "radius" "lexical (parameter)")
                  ("(make-point :x 0" "make-point" "defstruct constructor")
                  ("(shape-area (make-instance" "shape-area" "defclass accessor")
                  ("(shape-name s)" "shape-name" "defclass accessor")
                  ;; A USE site, not the defmethod header. Probing the header
                  ;; asks "go to the definition of this definition", which has a
                  ;; legitimately different answer -- and reading its NIL as a
                  ;; missing index entry is what put defgeneric in the
                  ;; not-indexed column of the review's first draft.
                  ("(describe-shape s))" "describe-shape" "defgeneric/defmethod")
                  ("(deftype small-int" "small-int" "deftype (at its definition)")))
    (destructuring-bind (needle token label) spec
      (let* ((line (line-of *specimen* needle))
             (col (col-of *specimen* needle token)))
        (if (null line)
            (note :info label "probe anchor not found -- specimen changed?")
            (let* ((r (result-of (funcall call "textDocument/definition"
                                          (dict "textDocument" (dict "uri" *uri*)
                                                "position" (dict "line" line
                                                                 "character" col))
                                          1)))
                   (n (count-items r)))
              (if (plusp n)
                  (note :ok label (format nil "~A resolved" token))
                  (note :gap label (format nil "~A did NOT resolve" token)))))))))

(defun probe-reference-scoping (call)
  (format t "~&~%== find-references, scope correctness ==~%")
  ;; AREA is bound three times over: a LET binding, a FLET parameter that
  ;; shadows it, and it also appears inside a string and inside a comment. A
  ;; scope-aware implementation reports the LET binding and its uses -- not the
  ;; shadowed parameter, and never the string or the comment.
  (let* ((line (line-of *specimen* "(let ((area (* radius radius"))
         (col (col-of *specimen* "(let ((area (* radius radius" "area"))
         (r (result-of (funcall call "textDocument/references"
                                (dict "textDocument" (dict "uri" *uri*)
                                      "position" (dict "line" line "character" col)
                                      "context" (dict "includeDeclaration" t))
                                1)))
         (n (count-items r))
         ;; Full ranges, not just line numbers. Comparing lines alone called two
         ;; genuine occurrences on one line a duplicate -- and line 32 really
         ;; does contain AREA twice, as the FLET parameter and its use.
         (ranges (when (vectorp r)
                   (map 'list (lambda (loc)
                                (let ((s (gethash "start" (gethash "range" loc))))
                                  (list (gethash "line" s) (gethash "character" s))))
                        r)))
         (lines (sort (mapcar #'first ranges) #'<)))
    (note :info "references to the LET-bound AREA" (format nil "~A result(s) on lines ~A" n lines))
    (let ((string-line (line-of *specimen* "the STRING"))
          (comment-line (line-of *specimen* "in a comment")))
      (when (member string-line lines)
        (note :bug "references" "includes the occurrence inside a STRING"))
      (when (member comment-line lines)
        (note :bug "references" "includes the occurrence inside a COMMENT"))
      (when (and ranges (/= (length ranges)
                            (length (remove-duplicates ranges :test #'equal))))
        (note :bug "references" "returns duplicate locations"))
      ;; The shadowing case, called out separately because it is the one thing
      ;; scope resolution cannot yet get right: FLET and LABELS create no scope,
      ;; so their parameters resolve to the outer binding.
      (let ((flet-line (line-of *specimen* "(flet ((scale (area)")))
        (when (member flet-line lines)
          (note :gap "references"
                "includes the shadowing FLET parameter (flet/labels create no scope)"))))))

(defun probe-simple (call label method params &key expect-nonempty)
  (let* ((response (handler-case (funcall call method params 1)
                     (error (e) (note :bug label (format nil "signalled: ~A" e))
                       nil)))
         (r (result-of response))
         (n (count-items r)))
    (cond
      ;; Not silence -- a NIL response to a REQUEST is itself wrong. The client
      ;; is waiting for an id-matched reply and will hang or time out. An earlier
      ;; version of this cond returned NIL here and signatureHelp simply vanished
      ;; from the report.
      ((null response) (note :bug label "no response at all to a request"))
      ((response-is-error-p response)
       (note :bug label "returned a JSON-RPC error response"))
      ((and expect-nonempty (zerop n)) (note :gap label "returned nothing"))
      (t (note :ok label (format nil "~A item(s)" n))))
    r))

(defun probe-other-operations (call)
  (format t "~&~%== the operations Claude Code's client cannot reach ==~%")
  (let ((fn-line (line-of *specimen* "(defun uses-everything")))
    (probe-simple call "completion"
                  "textDocument/completion"
                  (dict "textDocument" (dict "uri" *uri*)
                        "position" (dict "line" fn-line "character" 6))
                  :expect-nonempty t)
    (probe-simple call "signatureHelp"
                  "textDocument/signatureHelp"
                  (dict "textDocument" (dict "uri" *uri*)
                        "position" (dict "line" (line-of *specimen* "(make-point :x 0")
                                         "character" (+ 3 (col-of *specimen* "(make-point :x 0" "make-point"))))
                  :expect-nonempty nil)
    (probe-simple call "formatting"
                  "textDocument/formatting"
                  (dict "textDocument" (dict "uri" *uri*)
                        "options" (dict "tabSize" 2 "insertSpaces" t))
                  :expect-nonempty nil)
    (probe-simple call "documentHighlight"
                  "textDocument/documentHighlight"
                  (dict "textDocument" (dict "uri" *uri*)
                        "position" (dict "line" (line-of *specimen* "(let ((area (* radius radius")
                                         "character" (col-of *specimen* "(let ((area (* radius radius" "area")))
                  :expect-nonempty t)))

(defun probe-document-symbol (call)
  (format t "~&~%== documentSymbol, the file outline ==~%")
  (let* ((r (result-of (funcall call "textDocument/documentSymbol"
                                (dict "textDocument" (dict "uri" *uri*))
                                1))))
    (if (or (null r) (zerop (length r)))
        (note :gap "documentSymbol" "returned nothing")
        (progn
          (note :ok "documentSymbol" (format nil "~A symbol(s)" (length r)))
          (map nil (lambda (s)
                     (let* ((range (gethash "range" s))
                            (sel (gethash "selectionRange" s))
                            (rl (gethash "line" (gethash "start" range)))
                            (re (gethash "line" (gethash "end" range)))
                            (sl (gethash "line" (gethash "start" sel))))
                       ;; The spec requires selectionRange to be inside range.
                       (unless (and (<= rl sl) (<= sl re))
                         (note :bug "documentSymbol"
                               (format nil "~A: selectionRange outside range"
                                       (gethash "name" s))))
                       (format t "      ~2D-~2D  kind ~2D  ~A~%"
                               rl re (gethash "kind" s) (gethash "name" s))))
               r)))))

(defun probe-unregistered-methods (call)
  (format t "~&~%== methods a client will ask for ==~%")
  ;; What an editor or agent sends whether or not we advertise it. Anything
  ;; unhandled should at least fail cleanly rather than take the server down.
  (dolist (method '("textDocument/didClose"
                    "textDocument/rename"
                    "textDocument/prepareRename"
                    "textDocument/codeAction"
                    "textDocument/foldingRange"
                    "textDocument/selectionRange"
                    "textDocument/prepareCallHierarchy"
                    "textDocument/typeDefinition"
                    "textDocument/declaration"
                    "textDocument/implementation"
                    "textDocument/semanticTokens/full"
                    "textDocument/inlayHint"))
    (let ((response (handler-case
                        (funcall call method
                                 (dict "textDocument" (dict "uri" *uri*)
                                       "position" (dict "line" 0 "character" 0))
                                 1)
                      (error (e) (note :bug method (format nil "signalled: ~A" e)) :signalled))))
      (cond
        ((eq response :signalled) nil)
        ((response-is-error-p response) (note :gap method "not implemented (clean error)"))
        ((null response) (note :gap method "not implemented (no response)"))
        (t (note :ok method "handled"))))))

(defun run-sweep ()
  (with-direct-handler-test
    (init-server)
    (flet ((call (method params id) (call-handler method params :id id)))
      (open-specimen (lambda (m p id) (call-handler m p :id id)))
      (probe-definition-forms #'call)
      (probe-reference-scoping #'call)
      (probe-other-operations #'call)
      (probe-document-symbol #'call)
      (probe-unregistered-methods #'call)))

  (format t "~&~%========================================~%")
  (let ((bugs (count :bug *findings* :key #'first))
        (gaps (count :gap *findings* :key #'first)))
    (format t "~A bug(s), ~A gap(s)~%" bugs gaps)))

(run-sweep)
