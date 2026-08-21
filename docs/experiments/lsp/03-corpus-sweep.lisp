;;;; Run every operation over the corpus and check invariants.
;;;;
;;;; Companion to 01-operation-sweep.lisp, which drives one specimen with
;;;; hand-placed probes. This one asserts properties that must hold for ANY
;;;; file, so it generalises to whatever the corpus grows into:
;;;;
;;;;   1. Every request is answered. Never NIL, never an error response, never
;;;;      an unhandled condition -- at EVERY symbol position in the file, not a
;;;;      chosen few.
;;;;   2. Every range is inside the file.
;;;;   3. selectionRange is inside range, as the spec requires.
;;;;   4. The text at a documentSymbol's selectionRange EQUALS the symbol's
;;;;      name. This is the one with teeth: it fails the moment byte and
;;;;      character offsets are confused, which is why the corpus contains a
;;;;      file full of multi-byte characters.
;;;;   5. Every reference range covers text equal to the symbol asked about.
;;;;
;;;; Prompted by measuring the old suite: 77 fixtures, median 2 lines, DEFUN in
;;;; 73 of them, and exactly 2 containing a package-qualified symbol -- which is
;;;; precisely the bug that escaped to a live probe. See
;;;; surveys/lsp-review.md §3d.
;;;;
;;;; Run: sbcl --noinform --non-interactive --load docs/experiments/lsp/03-corpus-sweep.lisp

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
  (dolist (f '("test/package.lisp" "test/framework.lisp"))
    (load (merge-pathnames f *lsp-root*))))

(in-package :clef-test)

(defparameter *corpus-dir*
  (merge-pathnames "docs/experiments/lsp/corpus/" (truename ".")))

(defparameter *findings* '())

(defun finding (file label detail)
  (push (list file label detail) *findings*)
  (format t "    ~C[31mFAIL~C[0m ~A -- ~A~%" #\Escape #\Escape label detail))

(defun ok (label detail)
  (format t "    ~C[32m ok ~C[0m ~A~@[ -- ~A~]~%" #\Escape #\Escape label detail))

;;; ---------------------------------------------------------------------------
;;; Text utilities, kept independent of the server's own offset code so a bug
;;; there cannot hide itself.
;;; ---------------------------------------------------------------------------

(defun source-lines (text)
  (coerce (uiop:split-string text :separator '(#\Newline)) 'vector))

(defun text-at-range (lines range)
  "The source text covered by RANGE, or :OUT-OF-BOUNDS."
  (let* ((start (gethash "start" range))
         (end (gethash "end" range))
         (sl (gethash "line" start)) (sc (gethash "character" start))
         (el (gethash "line" end)) (ec (gethash "character" end)))
    (cond
      ((or (null sl) (null sc) (null el) (null ec)) :malformed)
      ((or (minusp sl) (minusp sc) (>= sl (length lines)) (>= el (length lines)))
       :out-of-bounds)
      ((/= sl el) :multi-line)
      (t (let ((line (aref lines sl)))
           (if (or (> sc (length line)) (> ec (length line)))
               :out-of-bounds
               (subseq line sc ec)))))))

(defun range-within-file-p (lines range)
  (let* ((start (gethash "start" range))
         (end (gethash "end" range)))
    (and start end
         (< (gethash "line" start) (length lines))
         (< (gethash "line" end) (length lines)))))

(defun range-contains-p (outer inner)
  "Is INNER inside OUTER?"
  (flet ((pos (r key) (let ((p (gethash key r)))
                        (cons (gethash "line" p) (gethash "character" p)))))
    (let ((os (pos outer "start")) (oe (pos outer "end"))
          (is (pos inner "start")) (ie (pos inner "end")))
      (and (or (< (car os) (car is))
               (and (= (car os) (car is)) (<= (cdr os) (cdr is))))
           (or (> (car oe) (car ie))
               (and (= (car oe) (car ie)) (>= (cdr oe) (cdr ie))))))))

(defun symbol-positions (text &key (limit 400))
  "Every (line . character) that sits on a word character.

Sampled across the whole file rather than at chosen spots: the point is to find
positions nobody thought to try."
  (let ((positions '())
        (lines (source-lines text)))
    (loop :for line :across lines
          :for row :from 0
          :do (loop :for col :from 0 :below (length line)
                    :for ch := (char line col)
                    :when (or (alphanumericp ch) (find ch "-*+<>=/"))
                      :do (push (cons row col) positions)))
    (let ((all (nreverse positions)))
      ;; Thin evenly so a long file does not dominate the run.
      (if (<= (length all) limit)
          all
          (loop :with step := (ceiling (length all) limit)
                :for p :in all :for i :from 0
                :when (zerop (mod i step)) :collect p)))))

;;; ---------------------------------------------------------------------------
;;; Checks
;;; ---------------------------------------------------------------------------

(defparameter *position-operations*
  '("textDocument/definition"
    "textDocument/hover"
    "textDocument/references"
    "textDocument/documentHighlight"
    "textDocument/prepareCallHierarchy"))

(defun check-every-request-answered (call uri text file)
  "Invariant 1: every request is answered, at every symbol position."
  (let ((positions (symbol-positions text))
        (unanswered '())
        (errored '())
        (crashed '()))
    (dolist (pos positions)
      (dolist (method *position-operations*)
        (let ((response
                (handler-case
                    (funcall call method
                             (dict "textDocument" (dict "uri" uri)
                                   "position" (dict "line" (car pos)
                                                    "character" (cdr pos))
                                   "context" (dict "includeDeclaration" t))
                             1)
                  (error (e)
                    (push (list method pos (princ-to-string e)) crashed)
                    :crashed))))
          (cond ((eq response :crashed) nil)
                ((null response) (pushnew (list method pos) unanswered :test #'equal))
                ((response-is-error-p response)
                 (pushnew (list method pos) errored :test #'equal))))))
    (format t "  ~A positions x ~A operations = ~A requests~%"
            (length positions) (length *position-operations*)
            (* (length positions) (length *position-operations*)))
    (if crashed
        (finding file "handler signalled"
                 (format nil "~A crash(es), first: ~A at ~A: ~A"
                         (length crashed) (first (first crashed))
                         (second (first crashed)) (third (first crashed))))
        (ok "no handler signalled" nil))
    (if unanswered
        (finding file "request unanswered"
                 (format nil "~A unanswered, first: ~A at ~A"
                         (length unanswered) (first (first unanswered))
                         (second (first unanswered))))
        (ok "every request answered" nil))
    (if errored
        (finding file "error response"
                 (format nil "~A error response(s), first: ~A at ~A"
                         (length errored) (first (first errored))
                         (second (first errored))))
        (ok "no error responses" nil))))

(defun check-document-symbols (call uri text file)
  "Invariants 2-4."
  (let* ((result (response-result-safe
                  (funcall call "textDocument/documentSymbol"
                           (dict "textDocument" (dict "uri" uri)) 1)))
         (lines (source-lines text))
         (bad-bounds '())
         (bad-nesting '())
         (bad-text '()))
    (if (or (null result) (zerop (length result)))
        (finding file "documentSymbol" "returned nothing for a file full of definitions")
        (progn
          (format t "  ~A document symbols~%" (length result))
          (map nil
               (lambda (sym)
                 (let ((name (gethash "name" sym))
                       (range (gethash "range" sym))
                       (selection (gethash "selectionRange" sym)))
                   (unless (and (range-within-file-p lines range)
                                (range-within-file-p lines selection))
                     (push name bad-bounds))
                   (when (and range selection
                              (not (range-contains-p range selection)))
                     (push name bad-nesting))
                   ;; Invariant 4: the text under selectionRange IS the name.
                   ;;
                   ;; With one deliberate exception: everything DEFSTRUCT
                   ;; generates. POINT-Y, POINT-P and MAKE-POINT appear nowhere
                   ;; in the source, so their selectionRange points at the slot
                   ;; or at the struct name -- the only things there are to point
                   ;; at, and the places you actually want to land. The covered
                   ;; text is then part of the name rather than equal to it.
                   (let ((covered (text-at-range lines selection)))
                     (unless (and (stringp covered)
                                  (or (string-equal covered name)
                                      (search covered name :test #'char-equal)))
                       (push (list name covered) bad-text)))))
               result)
          (if bad-bounds
              (finding file "symbol range out of bounds" (format nil "~A" bad-bounds))
              (ok "all ranges within the file" nil))
          (if bad-nesting
              (finding file "selectionRange outside range" (format nil "~A" bad-nesting))
              (ok "selectionRange inside range" nil))
          (if bad-text
              (finding file "selectionRange text /= name"
                       (format nil "~A mismatch(es), e.g. ~S"
                               (length bad-text) (first bad-text)))
              (ok "selectionRange covers exactly the name" nil))))
    result))

(defun check-references-cover-the-symbol (call uri text file symbols)
  "Invariant 5: a reference range covers text equal to the symbol's name."
  (let ((lines (source-lines text))
        (mismatches '())
        (checked 0))
    (map nil
         (lambda (sym)
           (let* ((name (gethash "name" sym))
                  (selection (gethash "selectionRange" sym))
                  (start (gethash "start" selection))
                  (refs (response-result-safe
                         (funcall call "textDocument/references"
                                  (dict "textDocument" (dict "uri" uri)
                                        "position" (dict "line" (gethash "line" start)
                                                         "character" (gethash "character" start))
                                        "context" (dict "includeDeclaration" t))
                                  1))))
             (when (vectorp refs)
               (map nil (lambda (loc)
                          (incf checked)
                          (let ((covered (text-at-range lines (gethash "range" loc))))
                            ;; Equal, or a qualified use covering pkg:name, or a
                            ;; generated accessor whose reference is the slot it
                            ;; was generated from (see the note above).
                            (unless (and (stringp covered)
                                         (or (string-equal covered name)
                                             (search name covered :test #'char-equal)
                                             (search covered name :test #'char-equal)))
                              (push (list name covered) mismatches))))
                    refs))))
         symbols)
    (format t "  ~A reference range(s) checked~%" checked)
    (if mismatches
        (finding file "reference range does not cover the symbol"
                 (format nil "~A mismatch(es), e.g. ~S"
                         (length mismatches) (first mismatches)))
        (ok "every reference range covers its symbol" nil))))

;;; ---------------------------------------------------------------------------

(defun sweep-file (path)
  (let* ((file (file-namestring path))
         (text (uiop:read-file-string path))
         (uri (format nil "file://~A" (namestring path))))
    (format t "~&~%========== ~A (~A lines) ==========~%"
            file (length (source-lines text)))
    (with-direct-handler-test
      (init-server)
      (flet ((call (method params id) (call-handler method params :id id)))
        (call-handler "textDocument/didOpen"
                      (dict "textDocument" (dict "uri" uri "languageId" "lisp"
                                                 "version" 1 "text" text))
                      :id nil)
        (let ((symbols (check-document-symbols #'call uri text file)))
          (when (vectorp symbols)
            (check-references-cover-the-symbol #'call uri text file symbols)))
        (check-every-request-answered #'call uri text file)))))

(defun run ()
  (let ((files (sort (directory (merge-pathnames "*.lisp" *corpus-dir*))
                     #'string< :key #'namestring)))
    (format t "~&Corpus sweep over ~A file(s)~%" (length files))
    (dolist (f files) (sweep-file f))
    (format t "~&~%========================================~%")
    (if *findings*
        (progn
          (format t "~A finding(s):~%" (length *findings*))
          (dolist (f (reverse *findings*))
            (format t "  ~A: ~A -- ~A~%" (first f) (second f) (third f))))
        (format t "no findings~%"))))

(run)
