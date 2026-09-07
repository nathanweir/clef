(defpackage :clef-conditions/src/render
  (:use :cl)
  (:import-from :clef-conditions/src/extract
                #:diagnostic-severity
                #:diagnostic-kind
                #:diagnostic-symbol
                #:diagnostic-message
                #:diagnostic-file
                #:diagnostic-file-position
                #:diagnostic-context
                #:diagnostic-source-form
                #:diagnostic-references)
  (:export
   #:render
   #:render-to-string
   #:*color*
   #:*context-lines*
   ;; Extension point: kinds whose FILE-POSITION is the exact answer, so the
   ;; renderer draws a caret without the "location is approximate" note.
   ;; Exported for clef-lint, whose positions come from a parse tree and are
   ;; always exact.
   #:*exactly-located-kinds*))

(in-package :clef-conditions/src/render)

;;;; Render a DIAGNOSTIC the way a modern toolchain would: the message first,
;;;; then where it happened, then the offending line with the problem marked.
;;;;
;;;; SBCL's own output buries the message under a macroexpansion dump and a
;;;; backtrace. Everything needed to do better is already in the diagnostic --
;;;; see docs/surveys/w0-conditions.md.
;;;;
;;;; Colour is opt-in and off by default. The language server's protocol stream
;;;; must carry nothing but framing, so the renderer returns text and only a
;;;; terminal front-end turns colour on.

(defparameter *color* nil
  "When true, RENDER emits ANSI colour. Off by default: callers writing to an
LSP protocol stream must never emit escape codes.")

(defparameter *context-lines* 0
  "How many source lines of context to show either side of the offending line.")

(defun ansi (code text)
  (if *color*
      (format nil "~C[~Am~A~C[0m" #\Escape code text #\Escape)
      text))

(defun severity-label (severity)
  (ecase severity
    (:error        (ansi "31;1" "error"))
    (:warning      (ansi "33;1" "warning"))
    (:style-warning (ansi "36;1" "style"))
    (:note         (ansi "34;1" "note"))))

;;; ---------------------------------------------------------------------------
;;; Byte offsets to line/column
;;;
;;; DIAGNOSTIC-FILE-POSITION is a byte offset, so the file is read as octets and
;;; decoded per line. Doing this on a decoded string would misplace the caret in
;;; any file containing non-ASCII above the error.
;;; ---------------------------------------------------------------------------

(defun read-file-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (when s
      (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
        (read-sequence buf s)
        buf))))

(defun octets-to-string* (octets &key (start 0) (end (length octets)))
  (handler-case (sb-ext:octets-to-string octets :external-format :utf-8
                                                :start start :end end)
    (error ()
      ;; Undecodable bytes shouldn't cost us the diagnostic.
      (map 'string #'code-char (subseq octets start end)))))

(defun locate (octets offset)
  "Return (values line-number column line-start line-end), all 1-based for line
and 0-based for column, measured in bytes."
  (let ((line 1)
        (line-start 0)
        (len (length octets)))
    (loop for i from 0 below (min offset len)
          when (= (aref octets i) 10)
            do (incf line) (setf line-start (1+ i)))
    (let ((line-end (or (position 10 octets :start line-start) len)))
      (values line (- offset line-start) line-start line-end))))

;;; ---------------------------------------------------------------------------
;;; Narrowing to the offending symbol
;;;
;;; IMPORTANT, and not what it first appears: DIAGNOSTIC-FILE-POSITION locates
;;; the enclosing TOP-LEVEL FORM, not the error. Measured -- three different
;;; conditions inside one defun all reported file-position 42, the position of
;;; the defun itself. What distinguishes them is ORIGINAL-SOURCE-PATH, a
;;; structural path into the form ((2) vs (3 2)).
;;;
;;; Resolving that path properly means re-reading the form with position
;;; tracking, or mapping the path onto a syntax tree. The language server does
;;; the latter -- it walks the path into its tree-sitter tree and gets the exact
;;; subexpression. This standalone renderer has no tree and does not want to
;;; grow a parser, so it does the cheap thing below.
;;;
;;; So: scan forward from the top-level form for the symbol we already know from
;;; FORMAT-ARGUMENTS. Bounded by the form, this is accurate for ordinary code and
;;; degrades to pointing at the form. It is emphatically better than searching
;;; the whole file, which is what clef's old diagnostics did -- that flagged
;;; every occurrence of the name anywhere in the file, correct uses included.
;;; ---------------------------------------------------------------------------

(defparameter *exactly-located-kinds* '(:unclosed-form :unmatched-paren)
  "Kinds whose FILE-POSITION is already the exact answer.

For everything else the position names the enclosing top-level form and the
renderer says so, rather than letting a caret imply a precision it does not have.
But a reader error is different in kind: the caret for an unmatched close paren
sits on that very paren, and for an unclosed form it sits on the open paren that
was never closed -- which is not a fallback, it is the thing to point at.
Measured in docs/experiments/conditions/03-reader-error-api.lisp.")

(defun char-eq-ci (a b)
  (let ((ca (code-char a)) (cb (code-char b)))
    (char-equal ca cb)))

(defun byte-search-ci (needle octets start end)
  "Case-insensitive search for NEEDLE (a string) in OCTETS between START and END.
Returns the byte offset or NIL."
  (let ((n (length needle))
        (limit (min end (length octets))))
    (when (plusp n)
      (loop for i from (max 0 start) to (- limit n)
            when (loop for j from 0 below n
                       always (char-eq-ci (aref octets (+ i j)) (char-code (char needle j))))
              return i))))

(defun narrow-to-symbol (octets form-offset symbol &key (window 4096))
  "Byte offset of SYMBOL at or after FORM-OFFSET, or NIL.

WINDOW bounds the search so a symbol that does not actually appear in this form
cannot drag the caret into an unrelated later definition."
  (when (and symbol octets form-offset)
    (byte-search-ci (symbol-name symbol) octets form-offset
                    (min (length octets) (+ form-offset window)))))

(defun forward-to-code (octets offset)
  "OFFSET advanced past whitespace and comments to the next code byte.

COMPILER-ERROR-CONTEXT-FILE-POSITION is where the READ of the enclosing
top-level form began -- which is immediately after the PREVIOUS form, before
any whitespace, blank lines or comments in between. Rendered as-is, the caret
for a diagnostic with no symbol to narrow to landed on the end of the previous
form: a type warning for line 5's (+ \"a string\" 1) pointed at line 2's
(in-package :p2). The form the compiler means starts at the next code byte.

A no-op when OFFSET already sits on code, so it is safe to apply uniformly --
reader errors' form-start positions point directly at a paren and pass through
unchanged."
  (let ((len (length octets)))
    (loop
      (cond
        ((>= offset len) (return offset))
        ;; Whitespace: space, tab, newline, return, page.
        ((member (aref octets offset) '(32 9 10 13 12)) (incf offset))
        ;; Line comment: to end of line, then continue skipping.
        ((= (aref octets offset) #.(char-code #\;))
         (setf offset (or (position 10 octets :start offset) len)))
        ;; Block comment: #| ... |#, and they NEST in Common Lisp.
        ((and (< (1+ offset) len)
              (= (aref octets offset) #.(char-code #\#))
              (= (aref octets (1+ offset)) #.(char-code #\|)))
         (let ((depth 1) (i (+ offset 2)))
           (loop while (and (plusp depth) (< (1+ i) len))
                 do (cond ((and (= (aref octets i) #.(char-code #\#))
                                (= (aref octets (1+ i)) #.(char-code #\|)))
                           (incf depth) (incf i 2))
                          ((and (= (aref octets i) #.(char-code #\|))
                                (= (aref octets (1+ i)) #.(char-code #\#)))
                           (decf depth) (incf i 2))
                          (t (incf i))))
           ;; An unterminated block comment: stop rather than loop forever.
           (if (plusp depth) (return offset) (setf offset i))))
        (t (return offset))))))

;;; ---------------------------------------------------------------------------
;;; Message
;;; ---------------------------------------------------------------------------

(defun headline (diag)
  "The message, minus SBCL's \"See also:\" trailer -- that is carried
structurally in REFERENCES and gets its own line."
  (let* ((msg (diagnostic-message diag))
         (cut (search "See also:" msg)))
    (string-right-trim '(#\Space #\Newline)
                       (if cut (subseq msg 0 cut) msg))))

(defun format-reference (ref)
  "SBCL references look like (SBCL NODE \"Handling of Types\")."
  (if (listp ref)
      (format nil "~{~A~^ / ~}" (mapcar #'princ-to-string ref))
      (princ-to-string ref)))

;;; ---------------------------------------------------------------------------
;;; Entry point
;;; ---------------------------------------------------------------------------

(defun render (diag &key (stream *standard-output*) source)
  "Write DIAG to STREAM in a human-readable form.

SOURCE may be the file's contents as an octet vector; when omitted the file named
by the diagnostic is read. A diagnostic with no location still renders -- it just
shows the message and whatever context it has."
  (let* ((label (severity-label (diagnostic-severity diag)))
         (file (diagnostic-file diag))
         (offset (diagnostic-file-position diag))
         (octets (or source (when file (ignore-errors (read-file-octets file))))))

    (format stream "~&~A: ~A~%" label (headline diag))

    (if (and octets offset (< offset (length octets)))
        (let* ((offset (forward-to-code octets offset))
               (sym (diagnostic-symbol diag))
               (precise (narrow-to-symbol octets offset sym))
               (mark-width (if precise (length (symbol-name sym)) 1))
               (at (or precise offset)))
          (multiple-value-bind (line column line-start line-end) (locate octets at)
            (let* ((line-text (octets-to-string* octets :start line-start :end line-end))
                   (gutter (format nil "~D" line))
                   (pad (make-string (length gutter) :initial-element #\Space)))
              (format stream "~A--> ~A:~D:~D~%" (concatenate 'string pad "  ")
                      (or file "<unknown>") line (1+ column))
              (format stream "~A |~%" pad)
              (format stream "~A | ~A~%" gutter line-text)
              (format stream "~A | ~A~A~%"
                      pad
                      (make-string column :initial-element #\Space)
                      (ansi "31;1" (make-string mark-width :initial-element #\^)))
              (format stream "~A |~%" pad)
              ;; Enclosing definition, when the compiler told us one.
              (let ((ctx (diagnostic-context diag)))
                (when ctx
                  (format stream "~A = in ~A~%" pad
                          (if (and (listp ctx) (= 1 (length ctx)))
                              (first ctx)
                              ctx))))
              ;; When we could not pin the symbol down, say so rather than
              ;; letting a caret on the form's first character imply precision.
              (unless (or precise
                          (member (diagnostic-kind diag) *exactly-located-kinds*))
                (format stream "~A = ~A~%" pad
                        "(location is the enclosing form; exact position unavailable)"))
              (dolist (ref (diagnostic-references diag))
                (format stream "~A = see ~A~%" pad (format-reference ref))))))
        ;; No location at all: still say what we know.
        (progn
          (when file (format stream "  --> ~A~%" file))
          (let ((form (diagnostic-source-form diag)))
            (when form (format stream "  in ~A~%" form)))))
    (values)))

(defun render-to-string (diag &key source)
  (with-output-to-string (s) (render diag :stream s :source source)))
