;;;; One-shot generator for the LSP package migration (W3 trial, step 2).
;;;;
;;;; RECORD, NOT A TOOL. This is the program that rewrote lsp/ from one
;;;; hand-ordered .asd and a packages.lisp monolith into 57 path-named
;;;; packages, kept so the rules it applied are inspectable (see
;;;; docs/surveys/w3-migration-trial.md). It ran against the tree at commit
;;;; 5b3289d and will not run cleanly against anything later: packages.lisp,
;;;; which it reads, no longer exists.
;;;;
;;;; Ran as: sbcl --noinform --non-interactive --load tmp/migrate-lsp.lisp
;;;; after `git checkout HEAD -- lsp docs' each time it needed adjusting.
;;;;
;;;; For every source and test file: replace the leading (in-package :OLD)
;;;; with a DEFPACKAGE named by the file's path, whose clauses are derived
;;;; from what the file actually references:
;;;;   - unqualified symbols inherited from a :USEd package, or listed in an
;;;;     old :IMPORT-FROM, or defined in another file of the same old package
;;;;     become :IMPORT-FROM the file that defines them;
;;;;   - qualified references (clef-symbols:foo, cl-ppcre:scan, ...) are
;;;;     rewritten to short local nicknames, one per defining file;
;;;;   - the old package's exports are split among the files that own them.
;;;; Scripts and experiments get full package names instead of nicknames.

(ql:quickload '(:cl-ppcre) :silent t)

(defpackage :migrate-lsp
  (:use :cl)
  (:local-nicknames (:re :cl-ppcre)))
(in-package :migrate-lsp)

(defparameter *repo* #p"/home/nathan/dev/clef/")
(defparameter *lsp* (merge-pathnames "lsp/" *repo*))
(defparameter *system* "clef-lsp")
(defparameter *warnings* '())
(defun note (fmt &rest args) (push (apply #'format nil fmt args) *warnings*))

;;; ---------------------------------------------------------------------------
;;; Files
;;; ---------------------------------------------------------------------------

(defstruct file
  rel          ; "src/util.lisp"
  path         ; absolute pathname
  old          ; old package name, lowercase string, or NIL for scripts
  new          ; new package name
  nick         ; local nickname consumers use
  text         ; original text
  tokens       ; hash set of unqualified tokens
  defs         ; hash set of exact definition names
  structs      ; list of defstruct names
  mode)        ; :module or :script

(defparameter *nick-overrides*
  '(("src/context" . "ctx")
    ("src/jsonrpc/types" . "rpc")
    ("src/parser/utils" . "parser-utils")
    ("src/lsp/types/base/types" . "base")
    ("src/lsp/types/basic/semantic-legend" . "legend")
    ("src/symbols/types" . "sym")
    ("src/symbols/init" . "symbols")
    ("src/lsp/server-capabilities" . "capabilities")
    ("src/lsp/workspace/diagnostic" . "ws-diagnostic")
    ("src/lsp/workspace/did-change-configuration" . "ws-config")
    ("src/lsp/workspace/symbol" . "ws-symbol")))

(defun rel-sans-type (rel)
  (subseq rel 0 (- (length rel) 5)))

(defun nick-for (rel)
  (let ((stem (rel-sans-type rel)))
    (or (cdr (assoc stem *nick-overrides* :test #'string=))
        (subseq stem (1+ (or (position #\/ stem :from-end t) -1))))))

(defun lisp-files-under (dir)
  (let ((out '()))
    (uiop:collect-sub*directories
     dir (constantly t)
     (lambda (d) (not (member (car (last (pathname-directory d)))
                              '("tmp" "build" ".claude") :test #'equal)))
     (lambda (d) (dolist (f (uiop:directory-files d "*.lisp")) (push f out))))
    (sort out #'string< :key #'namestring)))

(defun first-in-package (text)
  (let ((m (nth-value 1 (re:scan-to-strings "(?m)^\\(in-package\\s+:?([^\\s()]+)\\)" text))))
    (and m (string-downcase (aref m 0)))))

(defun load-file (path rel mode)
  (let* ((text (uiop:read-file-string path))
         (old (and (eq mode :module) (first-in-package text))))
    (make-file :rel rel :path path :old old
               :new (format nil "~A/~A" *system* (rel-sans-type rel))
               :nick (nick-for rel)
               :text text :mode mode)))

;;; ---------------------------------------------------------------------------
;;; Old package specs, read from packages.lisp and test/package.lisp
;;; ---------------------------------------------------------------------------

(defstruct spec name uses imports nicknames exports shadow)

(defparameter *specs* (make-hash-table :test #'equal))

(defun pkg-string (x) (string-downcase (string x)))

(defun read-specs (path)
  (let ((*package* (make-package (gensym "SCRATCH") :use '(:cl))))
    (with-open-file (in path)
      (loop for form = (read in nil nil) while form
            when (and (consp form) (string-equal (symbol-name (first form)) "DEFPACKAGE"))
              do (let ((spec (make-spec :name (pkg-string (second form)))))
                   (dolist (clause (cddr form))
                     (case (intern (symbol-name (first clause)) :keyword)
                       (:use (setf (spec-uses spec) (mapcar #'pkg-string (rest clause))))
                       (:import-from (push (mapcar #'pkg-string (rest clause)) (spec-imports spec)))
                       (:local-nicknames (setf (spec-nicknames spec)
                                               (mapcar (lambda (e) (mapcar #'pkg-string e)) (rest clause))))
                       (:export (setf (spec-exports spec) (mapcar #'pkg-string (rest clause))))
                       (:shadow (setf (spec-shadow spec) (mapcar #'pkg-string (rest clause))))))
                   (setf (gethash (spec-name spec) *specs*) spec))))))

;;; ---------------------------------------------------------------------------
;;; Definitions and tokens
;;; ---------------------------------------------------------------------------

(defun strip-noise (text)
  "TEXT with line comments, block comments, string literals and character
literals blanked to spaces. Length-preserving, so positions in the result
are positions in TEXT."
  (let ((out (make-string (length text) :initial-element #\Space))
        (i 0) (n (length text)))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond
                 ((char= c #\;)
                  (loop while (and (< i n) (char/= (char text i) #\Newline)) do (incf i)))
                 ((char= c #\")
                  (incf i)
                  (loop while (and (< i n) (char/= (char text i) #\"))
                        do (when (char= (char text i) #\\) (incf i))
                           (incf i))
                  (incf i))
                 ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
                  (let ((depth 1)) (incf i 2)
                    (loop while (and (plusp depth) (< (1+ i) n))
                          do (cond ((and (char= (char text i) #\#) (char= (char text (1+ i)) #\|))
                                    (incf depth) (incf i 2))
                                   ((and (char= (char text i) #\|) (char= (char text (1+ i)) #\#))
                                    (decf depth) (incf i 2))
                                   (t (incf i))))))
                 ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\\))
                  ;; character literal: skip #\x (and #\Space etc. roughly)
                  (incf i 3)
                  (loop while (and (< i n) (alpha-char-p (char text i))) do (incf i)))
                 (t (when (< i n) (setf (char out i) c)) (incf i)))))
    out))

(defparameter +delims+ (coerce '(#\( #\) #\' #\` #\, #\# #\" #\Space #\Tab #\Newline #\Return #\Page #\|) 'string))

(defun tokens-of (text)
  (let ((set (make-hash-table :test #'equal))
        (clean (strip-noise text)))
    (dolist (tok (re:split (format nil "[~A]+" (re:quote-meta-chars +delims+)) clean))
      (when (and (plusp (length tok)) (not (find #\: tok)))
        (setf (gethash (string-downcase tok) set) t)))
    set))

(defun scan-defs (file)
  (let ((defs (make-hash-table :test #'equal))
        (structs '())
        (text (strip-noise (file-text file))))
    (re:do-register-groups (kind name)
        ("(?m)^\\((defun|defmacro|defvar|defparameter|defconstant|defstruct|defclass|defgeneric|defmethod|deftype|define-condition|define-symbol-macro)\\s+\\(?([^\\s()]+)" text)
      (let ((name (string-downcase name)))
        (setf (gethash name defs) t)
        (when (member kind '("defstruct" "defclass" "define-condition") :test #'string=)
          (push name structs))))
    ;; CLOS accessors and struct-like readers declared inline.
    (re:do-register-groups (name) ("(?::accessor|:reader|:writer)\\s+([^\\s()]+)" text)
      (setf (gethash (string-downcase name) defs) t))
    (dolist (name (rest (assoc (file-rel file) *extra-defs* :test #'string=)))
      (setf (gethash name defs) t))
    (setf (file-defs file) defs
          (file-structs file) structs
          (file-tokens file) (tokens-of (file-text file)))))

;; Names a file "defines" without a top-level definer: local functions bound
;; by a macro's expansion (FLET inside WITH-DIRECT-HANDLER-TEST and
;; WITH-TEST-SERVER). Callers in other files must use the macro's own symbol,
;; so they import it like anything else.
(defparameter *extra-defs*
  '(("test/framework.lisp" "call-handler" "send-request")))
(defparameter *extra-exports*
  '(("test/framework.lisp" "call-handler" "send-request")))

(defun derived-from-struct-p (name struct)
  (or (string= name (format nil "make-~A" struct))
      (string= name (format nil "copy-~A" struct))
      (string= name (format nil "~A-p" struct))
      (and (> (length name) (1+ (length struct)))
           (string= struct name :end2 (length struct))
           (char= (char name (length struct)) #\-))))

;;; ---------------------------------------------------------------------------
;;; Ownership: which file of an old package defines NAME
;;; ---------------------------------------------------------------------------

(defparameter *files* '())

(defun files-of (old) (remove old *files* :key #'file-old :test-not #'equal))

(defun owner (old name)
  (let ((files (files-of old)))
    (cond
      ((null files) nil)
      ((null (rest files)) (first files))
      (t (or (find-if (lambda (f) (gethash name (file-defs f))) files)
             (find-if (lambda (f) (some (lambda (s) (derived-from-struct-p name s)) (file-structs f)))
                      files)
             ;; No file defines it: a reference in prose, or to a local
             ;; function. Leave the text alone rather than guess.
             (progn (note "no owner for ~A:~A; left untouched" old name)
                    nil))))))

;;; ---------------------------------------------------------------------------
;;; Qualified-reference rewriting
;;; ---------------------------------------------------------------------------

;; external prefix -> (nickname . canonical package). A nickname equal to the
;; canonical name means "declare with an empty :import-from, no nickname".
(defparameter *externals*
  '(("cl-ppcre" "ppcre" "cl-ppcre")
    ("ppcre" "ppcre" "cl-ppcre")
    ("ts" "ts" "cl-tree-sitter")
    ("cl-tree-sitter/high-level" "ts" "cl-tree-sitter")
    ("cl-tree-sitter" "ts" "cl-tree-sitter")
    ("interval" "interval" "interval")
    ("babel" "babel" "babel")
    ("serapeum" "serapeum" "serapeum")
    ("com.inuoe.jzon" "jzon" "com.inuoe.jzon")
    ("bordeaux-threads" "bt" "bordeaux-threads")
    ("bt" "bt" "bordeaux-threads")
    ("change-case" "change-case" "cl-change-case")
    ("cl-change-case" "change-case" "cl-change-case")
    ("indentify" "indentify" "indentify")
    ;; The sibling library: its public name is the system name.
    ("clef-conditions" "clef-conditions" "clef-conditions")))

(defparameter +qualified-re+
  "(?<![a-zA-Z0-9*+<>=/.:_!?%&-])([a-zA-Z][a-zA-Z0-9./_-]*)(::?)([a-zA-Z0-9*+<>=/_!?%&-]+)")

(defun rewrite-qualified (file &key full-names)
  "Rewrite FILE's text. Returns (values new-text nicknames-alist externals-list)
where nicknames are (nick . package) and externals are canonical packages
declared by bare :import-from."
  (let ((nicks '()) (externals '())
        (code (strip-noise (file-text file))))
    (flet ((use-nick (nick pkg)
             (if (string= nick pkg)
                 (pushnew pkg externals :test #'string=)
                 (pushnew (cons nick pkg) nicks :test #'equal)))
           (match-case (prefix replacement)
             ;; Prose written in upper case keeps its case.
             (if (some #'upper-case-p prefix) (string-upcase replacement) replacement)))
      (let ((new (re:regex-replace-all
                  +qualified-re+ (file-text file)
                  (lambda (target start end mstart mend rstarts rends)
                    (declare (ignore start end mend))
                    (let* ((prefix (subseq target (aref rstarts 0) (aref rends 0)))
                           (sep (subseq target (aref rstarts 1) (aref rends 1)))
                           (name (subseq target (aref rstarts 2) (aref rends 2)))
                           (lprefix (string-downcase prefix))
                           (lname (string-downcase name))
                           ;; In a comment or string? Then rewrite to the full
                           ;; new name for the reader's sake, but record no
                           ;; nickname: prose must not create a dependency.
                           (prose (char= (char code mstart) #\Space))
                           (full (or full-names prose)))
                      (cond
                        ((string= lprefix "ctx")
                         (unless prose (use-nick "ctx" (format nil "~A/src/context" *system*)))
                         (format nil "ctx~A~A" sep name))
                        ((gethash lprefix *specs*)
                         (let ((o (owner lprefix lname)))
                           (cond
                             ((null o) (format nil "~A~A~A" prefix sep name))
                             ((and (eq o file) (not prose))
                              (note "~A refers to its own package as ~A:~A" (file-rel file) prefix name)
                              name)
                             (full (format nil "~A~A~A" (match-case prefix (file-new o)) sep name))
                             (t (use-nick (file-nick o) (file-new o))
                                (format nil "~A~A~A" (file-nick o) sep name)))))
                        ((assoc lprefix *externals* :test #'string=)
                         (destructuring-bind (nick canonical) (rest (assoc lprefix *externals* :test #'string=))
                           (if full
                               (format nil "~A~A~A" (match-case prefix canonical) sep name)
                               (progn (use-nick nick canonical)
                                      (format nil "~A~A~A" nick sep name)))))
                        (t (format nil "~A~A~A" prefix sep name))))))))
        (values new nicks externals)))))

;;; ---------------------------------------------------------------------------
;;; Import derivation
;;; ---------------------------------------------------------------------------

(defun add-import (table pkg name)
  (pushnew name (gethash pkg table) :test #'string=))

(defun derive-imports (file)
  "Hash: package-name -> list of symbol names to :import-from."
  (let* ((old (file-old file))
         (spec (gethash old *specs*))
         (tokens (file-tokens file))
         (defs (file-defs file))
         (imports (make-hash-table :test #'equal)))
    (flet ((uses-p (name) (and (gethash name tokens) (not (gethash name defs)))))
      ;; (a) inherited via :use from another clef package
      (dolist (u (spec-uses spec))
        (unless (member u '("cl" "common-lisp") :test #'string=)
          (let ((uspec (gethash u *specs*)))
            (if uspec
                (dolist (e (spec-exports uspec))
                  (when (uses-p e)
                    (let ((o (owner u e))) (when o (add-import imports (file-new o) e)))))
                (note "~A :uses unknown package ~A" (file-rel file) u)))))
      ;; (b) old :import-from clauses, filtered to what is used
      (dolist (clause (spec-imports spec))
        (destructuring-bind (pkg &rest syms) clause
          (dolist (s syms)
            (when (uses-p s)
              (if (gethash pkg *specs*)
                  (let ((o (owner pkg s))) (when o (add-import imports (file-new o) s)))
                  (let ((ext (assoc pkg *externals* :test #'string=)))
                    (add-import imports (if ext (third ext) pkg) s)))))))
      ;; (c) same old package, other files
      (dolist (g (files-of old))
        (unless (eq g file)
          (maphash (lambda (name v) (declare (ignore v))
                     (when (uses-p name) (add-import imports (file-new g) name)))
                   (file-defs g))
          (dolist (s (file-structs g))
            (maphash (lambda (tok v) (declare (ignore v))
                       (when (and (derived-from-struct-p tok s)
                                  (not (gethash tok defs))
                                  (not (some (lambda (h) (gethash tok (file-defs h))) (files-of old))))
                         (add-import imports (file-new g) tok)))
                     tokens)))))
    imports))

(defun derive-exports (file)
  (let ((spec (gethash (file-old file) *specs*)))
    (union (remove-if-not (lambda (e) (eq (owner (file-old file) e) file)) (spec-exports spec))
           (rest (assoc (file-rel file) *extra-exports* :test #'string=))
           :test #'string=)))

;;; ---------------------------------------------------------------------------
;;; Emission
;;; ---------------------------------------------------------------------------

(defun sorted-keys (table)
  (sort (loop for k being the hash-keys of table collect k) #'string<))

(defun emit-header (file imports nicks externals exports)
  (with-output-to-string (s)
    (format s "(defpackage :~A~%  (:use :cl)" (file-new file))
    (dolist (pkg (sort (copy-list externals) #'string<))
      (unless (gethash pkg imports)
        (format s "~%  (:import-from :~A)" pkg)))
    (dolist (pkg (sorted-keys imports))
      (let ((syms (sort (copy-list (gethash pkg imports)) #'string<)))
        (format s "~%  (:import-from :~A" pkg)
        (let ((col 0))
          (dolist (sym syms)
            (when (> (+ col (length sym) 4) 60)
              (format s "~%               ") (setf col 0))
            (format s " #:~A" sym) (incf col (+ (length sym) 4))))
        (format s ")")))
    (when nicks
      (format s "~%  (:local-nicknames")
      (dolist (n (sort (copy-list nicks) #'string< :key #'car))
        (format s "~%    (:~A :~A)" (car n) (cdr n)))
      (format s ")"))
    (when exports
      (format s "~%  (:export")
      (dolist (e (sort (copy-list exports) #'string<))
        (format s "~%   #:~A" e))
      (format s ")"))
    (format s ")~%~%(in-package :~A)" (file-new file))))

(defun migrate-module (file)
  (multiple-value-bind (new-text nicks externals) (rewrite-qualified file)
    ;; The nickname pass sees the ORIGINAL text; unqualified use of slog etc.
    ;; is handled by imports. Nicknames for packages also imported by name
    ;; are fine to keep.
    (let* ((imports (derive-imports file))
           (exports (derive-exports file))
           (header (emit-header file imports nicks externals exports))
           (body (re:regex-replace "(?m)^\\(in-package\\s+:?[^\\s()]+\\)" new-text header)))
      (with-open-file (out (file-path file) :direction :output :if-exists :supersede)
        (write-string body out)))))

(defun migrate-script (file &key package)
  "Rewrite qualified references with full names; optionally prepend a
path-named package header when PACKAGE is given."
  (let ((new-text (rewrite-qualified file :full-names t)))
    ;; Two experiments reach load.lisp's *repo-root* through CL-USER.
    ;; Keyword package designators (symbol-call, in-package) in scripts.
    (setf new-text (re:regex-replace-all ":clef-scaffold :load-template-files" new-text
                                         ":clef-lsp/src/scaffold :load-template-files"))
    (setf new-text (re:regex-replace-all "\\(in-package :clef-test\\)" new-text
                                         "(in-package :clef-lsp/test/framework)"))
    (setf new-text (re:regex-replace-all "\"test/package.lisp\" " new-text ""))
    ;; test/package.lisp is gone; the runner must not load it.
    (setf new-text (re:regex-replace "(?m)^\\s*\\(load \\(project-path \"test/package.lisp\"\\)\\)\\n" new-text ""))
    (when package
      ;; Insert the header before the first top-level form: the first line
      ;; that starts with ( or # rather than a comment or blank.
      (let ((pos (nth-value 0 (re:scan "(?m)^[(#]" new-text))))
        (setf new-text
              (concatenate 'string
                           (subseq new-text 0 pos)
                           (format nil "(defpackage :~A~%  (:use :cl))~%~%(in-package :~A)~%~%"
                                   package package)
                           (subseq new-text pos)))))
    (with-open-file (out (file-path file) :direction :output :if-exists :supersede)
      (write-string new-text out))))

;;; ---------------------------------------------------------------------------
;;; Main
;;; ---------------------------------------------------------------------------

(defun run ()
  (read-specs (merge-pathnames "src/packages.lisp" *lsp*))
  (read-specs (merge-pathnames "test/package.lisp" *lsp*))
  ;; Modules: everything under src/ except packages.lisp, everything under
  ;; test/ except package.lisp, run-tests.lisp and client.lisp.
  (dolist (p (lisp-files-under (merge-pathnames "src/" *lsp*)))
    (let ((rel (enough-namestring p *lsp*)))
      (unless (string= rel "src/packages.lisp")
        (push (load-file p rel :module) *files*))))
  (dolist (p (lisp-files-under (merge-pathnames "test/" *lsp*)))
    (let ((rel (enough-namestring p *lsp*)))
      (unless (member rel '("test/package.lisp" "test/run-tests.lisp" "test/client.lisp") :test #'string=)
        (push (load-file p rel :module) *files*))))
  (setf *files* (nreverse *files*))
  (dolist (f *files*)
    (unless (file-old f) (note "~A has no in-package; skipped" (file-rel f)))
    (when (file-old f) (scan-defs f)))
  (setf *files* (remove nil *files* :key #'file-old))
  ;; Modules
  (dolist (f *files*) (migrate-module f))
  ;; Scripts inside lsp/
  (dolist (entry '(("build.lisp" . "clef-lsp/build")
                   ("load.lisp" . "clef-lsp/load")
                   ("test/run-tests.lisp" . "clef-lsp/test/run-tests")
                   ("test/client.lisp" . "clef-lsp/test/client")))
    (let ((p (merge-pathnames (car entry) *lsp*)))
      (migrate-script (load-file p (car entry) :script) :package (cdr entry))))
  ;; Experiments and other scripts outside lsp/: full names, no header.
  (dolist (p (append (lisp-files-under (merge-pathnames "docs/experiments/" *repo*))))
    (let ((f (load-file p (enough-namestring p *repo*) :script)))
      (when (re:scan "\\bclef-(symbols|lsp|parser|context|util|log|root|jsonrpc|lint|scaffold)\\b" (file-text f))
        (migrate-script f))))
  (format t "~&Migrated ~D modules.~%" (length *files*))
  (dolist (w (reverse *warnings*)) (format t "~&  note: ~A~%" w)))

(run)
