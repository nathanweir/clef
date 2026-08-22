(in-package :clef-symbols)

;;; Symbol analysis.
;;;
;;; Persistent state (scope trees, symbol-ref trees, the workspace-wide symbol
;;; index, per-file line offset caches, the global scope) all live on
;;; CLEF-CONTEXT:*SERVER*. Transient state used while walking a parse tree --
;;; the current scope and current package -- remain as dynamic specials in
;;; this package; they are only meaningful inside a single
;;; BUILD-FILE-SYMBOL-MAP call and don't belong in the shared context.

(defparameter *current-scope* nil
              "The current lexical scope that is the context in which the current processed node is occurring")

(defparameter *current-package* nil "The name of the current package encountered when processing the file")

;;; Workspace symbol index management

(defun clear-workspace-symbol-index ()
  "Clear the entire workspace symbol index."
  (clrhash ctx:workspace-symbol-index))

(defun remove-file-from-workspace-index (file-path)
  "Remove all symbol definitions from FILE-PATH from the workspace index."
  (let ((index ctx:workspace-symbol-index))
    (maphash (lambda (symbol-name defs)
               (let ((filtered (remove-if (lambda (def)
                                            (let ((loc (symbol-definition-location def)))
                                              (and loc (string= (location-file-path loc) file-path))))
                                          defs)))
                 (if filtered
                     (setf (gethash symbol-name index) filtered)
                     (remhash symbol-name index))))
             index)))

(defun add-to-workspace-index (symbol-def)
  "Add a symbol definition to the workspace index for cross-file lookup."
  (let* ((name (symbol-definition-symbol-name symbol-def))
         (index ctx:workspace-symbol-index)
         (existing (gethash name index)))
    (setf (gethash name index)
          (cons symbol-def existing))))

(defun lookup-in-workspace-index (symbol-name)
  "Look up a symbol by name in the workspace index. Returns a list of matching definitions."
  (gethash symbol-name ctx:workspace-symbol-index))

;; No longer needed since we set these on the global scope directly
;; (defparameter *built-in-symbol-defs* nil
;;               "A list of symbol-definition's for built-in Common Lisp symbols.")

;; (defparameter *external-pkg-symbol-defs* nil
;;               "A list of symbol-definition's for external package symbols loaded from the .asd.")

(defun get-ref-for-doc-pos (file-path line char)
       "Gets the symbol reference name & lexical-scope for the given document position.
Note that symbol-ref can be nil if none is at the location"
       ;; (slog :debug ">>>>>>>>: ~A ~A ~A" file-path line char)
       (let* ((path (clef-util:cleanup-path file-path))
              (offset (line-char-to-byte-offset path line char))
              (symbol-refs (interval:find-all (gethash path ctx:symbol-refs) offset))
              ;; Also get the lexical scope by position, as symbol-refs may be nil
              (scopes (interval:find-all (gethash path ctx:lexical-scopes) offset)))
             ;; (slog :debug "Found symbol-defs at line ~A char ~A (offset ~A): ~A" line char offset symbol-defs)
             ;; (slog :debug ">>>> scope intervals found: ~A" scopes)
             ;; (values nil nil)))
             ;; Third value: the package in effect where the reference appears.
             ;; Callers use it to break ties in the name-keyed workspace index,
             ;; which otherwise returns whichever same-named definition happened
             ;; to be indexed first, from any package in any component.
             (values
               (when (and symbol-refs (consp symbol-refs))
                     (symbol-reference-symbol-name (clef-interval-data (first symbol-refs))))
               (clef-interval-data (first (last scopes)))
               (when (and symbol-refs (consp symbol-refs))
                     (symbol-reference-package-name (clef-interval-data (first symbol-refs)))))))
;;
;; (if (> (length symbol-refs) 0)
;; (let ((symbol-ref (clef-interval-data (first symbol-refs))))
;;      (values (symbol-reference-usage-scope symbol-ref)
;;              (symbol-reference-symbol-name symbol-ref)))
;; (values nil nil))))

;; TODO: It is VERY annoying (and inefficient) to have to do this, but I've been unable to pull the byte
;; offsets out of death:cl-tree-sitter's low level API as they don't seem to be exposed in any way by
;; the nodes the high-level API creates
(defun line-char-to-byte-offset (file-path line char)
       "Converts a line and character position to a byte offset."
       (let* ((line-offsets (gethash file-path ctx:document-line-offsets))
              (line-index line) ;; Convert to 0-based
              (char-index char))     ;; Already 0-based
             ;; Add the char offset to the pre-calculated line offset
             (+ (aref line-offsets line-index) char-index)))



;; See above; becuase death/cl-tree-sitter doesn't expose byte offsets, clef-parser/parser was written incredibly
;; naively in that it repeatedly recalculates line offsets which causes a massive amount of performance waste in this file
;; when getting node text. This function here has been rewritten to use the pre-calculated file line offsets used for the symbol map
;; Long-term we need to either utility-ize this kind of text seeking, or find a different way to use tree-sitter that actually exposes
;; the byte offsets directly.
(defun fast-node-text (node source file-path)
       (let* ((line-offsets (gethash file-path ctx:document-line-offsets))
              (start-row (node-start-point-row node))
              (start-col (node-start-point-column node))
              (end-row (node-end-point-row node))
              (end-col (node-end-point-column node))
              (start-abs (+ (aref line-offsets start-row) start-col))
              (end-abs (+ (aref line-offsets end-row) end-col))
              (len (length source)))
             ;; (slog :debug "fast-node-text ~A: start-abs ~A, end-abs ~A, len ~A" file-path start-abs end-abs len)
             ;; (slog :debug "result: ~A" (subseq source start-abs (min end-abs len)))
             ;; (slog :debug "slow-node-text: ~A" (clef-parser/parser:node-text node source))
             ;; (slog :debug "---")
             (subseq source start-abs (min end-abs len))))

(defun byte-offsets-for-node (file-path node)
       "Gets the start and end byte offsets for a node in a file."
       (let* ((start-line (clef-parser/parser:node-start-point-row node))
              (start-char (clef-parser/parser:node-start-point-column node))
              (end-line (clef-parser/parser:node-end-point-row node))
              (end-char (clef-parser/parser:node-end-point-column node))
              (start-byte (line-char-to-byte-offset file-path start-line start-char))
              (end-byte (line-char-to-byte-offset file-path end-line end-char)))
             (values start-byte end-byte)))

(defun location-for-node (file-path node)
       "Creates a location object for the given node in the file."
       (multiple-value-bind (start end) (byte-offsets-for-node file-path node)
                            (make-location
                              :file-path file-path
                              :start start
                              :end end)))

;; (defun calculate-line-lengths (file-source)
;;        "Calculates the lengths of each line in the given file source."
;;        (let ((lines (cl-ppcre:split #\Newline file-source))
;;              (lengths '()))
;;             (dolist (line lines)
;;                     (push (length line) lengths))
;;             (concatenate 'vector (nreverse lengths))))

(defun calculate-line-offsets (file-source)
       "Calculates the byte offsets relative file start of each line in the given file source."
       (let ((byte-offset 0))
            (let ((lines (cl-ppcre:split #\Newline file-source))
                  (lengths '()))
                 (dolist (line lines)
                         (push byte-offset lengths)
                         (incf byte-offset (+ (length line) 1)))
                 (concatenate 'vector (nreverse lengths)))))

(defparameter *index-excluded-directories*
  '(".git" ".direnv" "build" "tmp" "result" "node_modules" ".cache" ".qlot")
  "Directory names never descended into when scanning a workspace.

Pruning at the DIRECTORY level, not filtering the results afterwards, which is
the entire point. The old code enumerated every .lisp file under the root and
then discarded the .direnv ones -- but the cost is in the walk, not the filter.
Measured on this repository: the unpruned walk took **2175 ms** and found 229
files, 90 of them inside .direnv (a nix profile full of vendored Lisp sources)
and 29 under tmp/. The pruned walk takes **4 ms** and finds the 100 files that
are actually project source.

That 2 seconds was paid on every server start, before a single symbol was
indexed.")

(defun excluded-directory-p (directory)
  (let ((name (car (last (pathname-directory directory)))))
    (and (stringp name)
         (member name *index-excluded-directories* :test #'string=)
         t)))

(defun project-lisp-files (project-root)
       "Every .lisp file under PROJECT-ROOT that could plausibly be project source."
       (let ((files '()))
            (uiop:collect-sub*directories
              (uiop:ensure-directory-pathname project-root)
              (constantly t)
              (lambda (directory) (not (excluded-directory-p directory)))
              (lambda (directory)
                      (dolist (file (uiop:directory-files directory "*.lisp"))
                              (push file files))))
            (nreverse files)))

(defun filter-files (file-paths)
       "Kept for callers that already have a list of paths.

PROJECT-LISP-FILES prunes while walking and is what the scan uses; this only
catches anything that slipped through."
       (remove-if (lambda (path)
                          (some (lambda (excluded)
                                        (search (concatenate 'string "/" excluded "/")
                                                (namestring path)))
                                *index-excluded-directories*))
                  file-paths))

(defun build-project-symbol-map (project-root)
       ;; Clear the workspace symbol index before rebuilding
       (clear-workspace-symbol-index)

       ;; Init the global scope and load in builtins + externals
       (setf ctx:global-scope (make-lexical-scope
                                :kind :workspace
                                :location nil
                                :parent-scope nil
                                :symbol-definitions '()
                                ;; Should never actually receive values
                                :symbol-references (make-hash-table)
                                :child-scopes '()
                                :node nil))

       (load-common-lisp-builtin-symbols ctx:global-scope)
       (load-asd-external-packages ctx:global-scope)

       (slog :debug "Building symbol map at ~A" project-root)
       ;; Discover every .lisp file recursively under the root
       (let* ((filtered-files (project-lisp-files (clef-util:cleanup-path project-root))))
             (slog :debug "Found ~A valid Lisp files in workspace." (length filtered-files))
             ;; Process each file to extract symbols
             (dolist (file-path filtered-files)
                     (index-file-from-disk (namestring file-path)))))

(defun index-file-from-disk (file-path)
       "Read FILE-PATH and index it, recording when it was last written.

The recorded time is taken BEFORE reading, deliberately. Taking it after would
lose an edit that landed between the read and the stat -- the next check would
see a matching timestamp and never re-read."
       (let ((written (ignore-errors (file-write-date file-path))))
            (let ((source (ignore-errors (clef-util:read-file-text file-path))))
                 (when source
                       (build-file-symbol-map file-path source)
                       (setf (gethash file-path ctx:file-index-times) written)))))

(defun forget-file (file-path)
       "Drop everything recorded for a file that no longer exists."
       (remove-file-from-workspace-index file-path)
       (remhash file-path ctx:lexical-scopes)
       (remhash file-path ctx:symbol-refs)
       (remhash file-path ctx:document-line-offsets)
       (remhash file-path ctx:file-index-times))

(defun refresh-stale-index ()
       "Re-index files that changed on disk since clef last read them.

**Why this exists.** The index is only updated by didOpen and didChange, so it
only ever learns about files an editor opens. An agent editing files directly --
which is how most of this codebase now gets written -- goes nowhere near the
protocol, and clef never hears about a single change. Answers then decay
silently over a session: workspace symbol was observed reporting three functions
that had been deleted, at line numbers that by then held something else. A wrong
answer that looks right is worse than a missing one.

A file currently open in the editor is skipped: the client's in-memory copy is
authoritative and may hold unsaved edits, and it is already indexed.

Cost is why this can run on every request that consults the index. Stat-ing the
whole workspace takes well under a millisecond; the directory walk that finds
new files costs about 4 ms now that it prunes."
       (let ((root ctx:workspace-root))
            (when root
                  (let ((present (make-hash-table :test 'equal))
                        (refreshed 0))
                       (dolist (path (project-lisp-files (clef-util:cleanup-path root)))
                               (let* ((file-path (namestring path))
                                      (uri (clef-util:path-to-file-uri file-path))
                                      (open-in-editor (and uri (gethash uri ctx:documents)))
                                      (written (ignore-errors (file-write-date file-path)))
                                      (indexed (gethash file-path ctx:file-index-times)))
                                     (setf (gethash file-path present) t)
                                     (unless open-in-editor
                                             (when (or (null indexed)
                                                       (null written)
                                                       (/= written indexed))
                                                   (index-file-from-disk file-path)
                                                   (incf refreshed)))))
                       ;; Files that have gone away. Left in place they answer
                       ;; go-to-definition with a location in a file that is not
                       ;; there any more.
                       (let ((vanished '()))
                            (maphash (lambda (file-path time)
                                             (declare (ignore time))
                                             (unless (gethash file-path present)
                                                     (push file-path vanished)))
                                     ctx:file-index-times)
                            (dolist (file-path vanished)
                                    (forget-file file-path)
                                    (incf refreshed)))
                       (when (plusp refreshed)
                             (slog :debug "Re-indexed ~A stale file(s)" refreshed))
                       refreshed))))

(defun build-file-symbol-map (file-path file-source)
       (slog :debug "Processing file for symbol-map: ~A" file-path)

       ;; Remove any existing symbols from this file in the workspace index
       ;; (needed when re-processing files on save)
       (remove-file-from-workspace-index file-path)

       ;; Reset any previously found package names
       (setf *current-package* nil)

       ;; Calculate and store line lengths for this document
       (setf (gethash file-path ctx:document-line-offsets)
             (calculate-line-offsets file-source))

       ;; Init the interval trees
       (setf (gethash file-path ctx:symbol-refs)
             (interval:make-tree))
       (setf (gethash file-path ctx:lexical-scopes)
             (interval:make-tree))

       ;; Parse the file with tree-sitter and then walk the output tree to find
       ;; the current package, record symbol definitions, symbol references, and
       ;; lexical scopes
       (let ((parse-tree (clef-parser/parser:parse-string file-source)))
            ;; Create the initial lexical-scope
            (setf *current-scope*
                  (make-lexical-scope
                    :kind :document
                    ;; One past the end of the file, deliberately.
                    ;;
                    ;; The scope interval tree keeps only the first interval for
                    ;; any pair of bounds, and the document scope is inserted
                    ;; first. A file whose single top-level form spans the whole
                    ;; text -- no trailing newline, one DEFUN -- produced a
                    ;; scope with exactly these bounds and was silently dropped,
                    ;; so nothing inside it had a scope at all: go-to-definition
                    ;; on a parameter failed and local reference scoping fell
                    ;; back to the document. Small single-function files, and
                    ;; every file while its first function is being written.
                    ;; See docs/surveys/lsp-review.md §1.8.
                    :location (make-location
                                :file-path file-path
                                :start 0
                                :end (1+ (length file-source)))
                    :parent-scope ctx:global-scope
                    :symbol-definitions '()
                    :symbol-references (make-hash-table)
                    :child-scopes '()
                    :node parse-tree))

            ;; Append as a child-scope of the global scope
            (push *current-scope* (lexical-scope-child-scopes ctx:global-scope))
            ;; Store the document scope on the interval tree so it can be found by find-all
            (store-scope-on-interval-tree *current-scope* file-path)
            (labels ((walk (n)
                           (let ((previous-scope *current-scope*)
                                 (node-type (ts:node-type n)))
                                (when (or (eql node-type :error) (eql node-type :missing))
                                      ;; TODO: What to do on syntax errors? Just abort?
                                      '())
                                ;; (push (cons type (ts:node-range n)) results))
                                (progn
                                  ;; (slog :debug "build-symbol-map> node-type is: ~A" type)
                                  ;; Update current tracked package by looking for (in-package package-name)
                                  (check-for-in-package n node-type file-source file-path)
                                  ;; Look for forms that define new variables, thus creating a new lexical-scope
                                  ;; and relevant symbol-definition's
                                  (check-for-defun n node-type file-path file-source)
                                  (check-for-let-binding n node-type file-path file-source)
                                  (check-for-local-function-binding n node-type file-path file-source)
                                  (check-for-simple-define n node-type file-path file-source)
                                  ;; Type-defining forms. All plain :LIST-LITs,
                                  ;; unlike DEFUN which has a grammar node.
                                  (check-for-class-define n node-type file-path file-source)
                                  (check-for-struct-define n node-type file-path file-source)
                                  (check-for-type-define n node-type file-path file-source)
                                  ;; Fallback for DEF* forms with no dedicated
                                  ;; checker, including project-defined macros.
                                  (check-for-generic-define n node-type file-path file-source)
                                  ;; Check if the node is a symbol-reference and record it into the scope if so
                                  ;; (uses *current-scope* internally intead of passing a scope in here)
                                  (check-for-symbol-reference n node-type file-path file-source)

                                  (dolist (child (ts:node-children n))
                                          (walk child))
                                  ;; Restore previous scope
                                  (setf *current-scope* previous-scope)))))
                    (walk parse-tree))
            '()))

(defun load-external-symbols (is-loaded global-scope package)
       "Loads external package symbols into the global scope. 'is-loaded' sets whether the symbols
are available without package prefixing. Otherwise, symbols will be added as 'package-name:symbol-name"

       ;; (slog :debug "sideway-deps: ~A"
       ;;       (asdf:component-sideway-dependencies (asdf:find-system :clef)))

       (do-external-symbols (sym (find-package package))
                            (slog :debug "Making symbol-def for external symbol: ~A from package: ~A" sym package)
                            (let ((symbol-kind :unknown)
                                  (source-location nil))
                                 (when (special-operator-p sym)
                                       (setf symbol-kind :special-operator))
                                 (when (macro-function sym)
                                       (setf symbol-kind :macro))
                                 (when (fboundp sym)
                                       (setf symbol-kind :function))
                                 ;; (when (typep sym 'constant)
                                 ;;       (setf symbol-kind :constant))
                                 ;; (when (typep sym 'type)
                                 ;;       (setf symbol-kind :type))
                                 ;; (when (and (not symbol-kind)
                                 ;;            (boundp sym))
                                 ;;       (setf symbol-kind :variable)))

                                 (setf source-location
                                       (first (sb-introspect:find-definition-sources-by-name sym symbol-kind)))

                                 ;; TODO: It's probably better to not concat into the full name here,
                                 ;; and instead make that decision when emitting the symbols list.
                                 ;; That does require tracking an extra val of whether it's in scope
                                 (let* ((name (if is-loaded
                                                  (string-downcase (symbol-name sym))
                                                  (format nil "~A:~A"
                                                          (string-downcase (symbol-name package))
                                                          (string-downcase (symbol-name sym)))))
                                        (stringpath (if source-location
                                                        (namestring
                                                          (sb-introspect:definition-source-pathname
                                                            source-location))
                                                        nil))
                                        (symbol-def (make-symbol-definition
                                                      :symbol-name name
                                                      :package-name package
                                                      :kind symbol-kind
                                                      :location (if stringpath (make-location
                                                                                 :file-path stringpath
                                                                                 :start 0
                                                                                 :end 0)
                                                                    nil)
                                                      ;; :defining-scope nil)))
                                                      :defining-scope global-scope
                                                      :node nil)))
                                       (push symbol-def (lexical-scope-symbol-definitions global-scope))))))

(defun load-common-lisp-builtin-symbols (global-scope)
       "Loads common lisp built-in symbols into the global scope for the given file."
       (load-external-symbols t global-scope :COMMON-LISP))

(defun load-asd-external-packages (global-scope)
       "Reads the referenced external packages for this system from the .ASD file and loads
those packages' members into the symbol map"
       (let ((lib-names (parse-lib-names-from-asd)))
            (dolist (lib-name lib-names)
                    (slog :debug "Loading external package symbols for lib: ~A" lib-name)
                    ;; TODO: Should probably do this in a thread to not pollute the language server
                    ;; (asdf:load-system lib-name)
                    ;; Attempt to load external symbols for the package, but catch the error and
                    ;; continue if it fails
                    (handler-case
                      ;; For now, we have to skip loading packages without easily inferrable names
                      (load-external-symbols nil global-scope (if (stringp lib-name)
                                                                  (read-from-string lib-name)
                                                                  lib-name))
                      (error (e)
                             (slog :debug "Warning: Could not load external package symbols for ~A: ~A"
                                   lib-name e))))))



(defun normalize-dependency-name (dep)
  "Return the system name in DEP as a string, or NIL if it names no system.

   ASDF's dependency-def grammar is more than plain names:

     dependency-def := simple-component-name
                     | (:feature feature-expression dependency-def)
                     | (:version simple-component-name version-specifier)
                     | (:require module-name)

   Treating every entry as an atom made a perfectly valid .asd abort the whole
   project symbol map, so recognise the list forms -- recursing for :feature,
   whose real dependency is nested -- and quietly ignore anything unrecognised
   rather than signalling from inside initialize.

   Feature-conditional dependencies are reported unconditionally: clef offers
   completion rather than building, so indexing a system the current
   implementation would skip is friendlier than omitting its symbols."
  (typecase dep
    ;; Before SYMBOL: NIL is both a symbol and the empty list, and an empty or
    ;; stray dependency entry must not become a system named "NIL".
    (null nil)
    (string dep)
    (symbol (symbol-name dep))
    (cons (let* ((head (first dep))
                 (head-name (and (symbolp head) (symbol-name head))))
            (cond
              ((null head-name) nil)
              ((and (string-equal head-name "FEATURE") (third dep))
               (normalize-dependency-name (third dep)))
              ((and (or (string-equal head-name "VERSION")
                        (string-equal head-name "REQUIRE"))
                    (second dep))
               (normalize-dependency-name (second dep)))
              (t nil))))
    (t nil)))

(defun parse-lib-names-from-asd ()
  "Retrieves a combined list of library names from all loaded systems.
Aggregates :depends-on from all discovered .asd files and filters out local system names."
  (let ((all-deps '())
        (local-system-names '())
        (systems ctx:loaded-systems))
    ;; Collect all local system names and their dependencies
    (maphash (lambda (name sys-info)
               (push name local-system-names)
               (let ((deps (clef-symbols:system-info-dependencies sys-info)))
                 (dolist (dep deps)
                   (let ((dep-name (normalize-dependency-name dep)))
                     (when dep-name
                           (pushnew (string-downcase dep-name) all-deps
                                    :test #'string-equal))))))
             systems)
    ;; Filter out local system names (don't try to load our own systems as external)
    (let ((external-deps (set-difference all-deps local-system-names :test #'string-equal)))
      (slog :debug "[symbol init] Found ~A external dependencies from ~A system(s)"
            (length external-deps)
            (hash-table-count systems))
      ;; Convert back to symbols for compatibility with existing code
      (mapcar (lambda (s) (intern (string-upcase s) :keyword)) external-deps))))

(defun check-for-in-package (node node-type source file-path)
       "Checks if the given node is an in-package declaration and updates *current-package* if so"
       ;; For debug, print start and end byte offsets for this node

       ;; (multiple-value-bind (start end) (byte-offsets-for-node file-path node)
       ;;                      (slog :debug "node byte offsets: ~A ~A" start end))
       ;; (slog :debug "node byte offsets: ~A ~A" (byte-offsets-for-node file-path node))
       (unless (eq node-type :list-lit)
               (return-from check-for-in-package nil))

       (let ((text (fast-node-text node source file-path)))
            ;; (slog :debug "text is: ~A" text)
            ;; Look for (in-package ...) forms
            (when (and (eq node-type :LIST-LIT)
                       (search "in-package" text))
                  (handler-case
                    (let ((form (read-from-string text)))
                         (when (and (consp form)
                                    (eq (car form) 'in-package))
                               (progn
                                 ;; (slog :debug "checked node for in-package: type ~A, node ~A" (ts:node-type node) node)
                                 (setf *current-package* (second form)))))
                    (error () nil)))))

;; TODO: Check for the following types of definition nodes:
;; DEFUN, DEFMACRO, DEFPARAMETER, DESTRUCTRING-BIND
;; Also, LET, LET*, FLET
;; Others to consider in the future (?): DEFTYPE, DEFSPECIAL, DEFSTRUCT, DEFMETHOD, DEFCLASS
;; 'defun', 'defmacro', 'defgeneric', 'defmethod'

;; 'defun' is a bad name, but IIRC the parser calls at least some of these nodes "defun" nodes, even for
;; defmacro and lambdas
;; TODO: Technically, global defs can occur anywhere and should only modify the global scope. Currently this
;; will modify the "current" scope if you do something like put a defparameter inside a defun
(defun defun-keyword-kind (keyword)
       "The symbol kind for a DEFUN-shaped form, from its keyword.

The grammar gives DEFUN, DEFMACRO, DEFGENERIC, DEFMETHOD and LAMBDA one node
type between them, and this used to record :FUNCTION for all of them under a
TODO reading \"Calc specific kind\". Telling DEFMETHOD apart is what makes
textDocument/implementation answerable at all -- a generic's implementations are
exactly its methods -- and it also stops documentSymbol reporting every method as
a plain function."
       (cond ((null keyword) :function)
             ((string-equal keyword "defmacro") :macro)
             ((string-equal keyword "defmethod") :method)
             ;; DEFGENERIC stays :FUNCTION. LSP has no generic-function kind, and
             ;; a generic is the thing you call.
             (t :function)))

(defun check-for-defun (node node-type file-path source)
       "If a 'defun' node is found, unpack the specific type of node, name, and params into
symbol-definitions. Returns the created lexical-scope if applicable, nil otherwise."
       (when (not (eq node-type :defun))
             (return-from check-for-defun nil))

       (let* ((defun-header-n (first (ts:node-children node)))
              (defun-header-children (ts:node-children defun-header-n))
              (defun-type (fast-node-text (first defun-header-children) source file-path))
              (defun-name-n
                ;; lambdas by definition have no name
                (if (string= defun-type "lambda") nil (second defun-header-children)))
              (param-nodes
                (ts:node-children
                  (if (string= defun-type "lambda")
                      (second defun-header-children) (third defun-header-children))))
              (defs '())
              (scope (make-lexical-scope
                       ;; TODO: Calc specific kind
                       :kind :defun
                       :location (location-for-node file-path node)
                       :parent-scope *current-scope*
                       :symbol-definitions '()
                       :symbol-references (make-hash-table)
                       :child-scopes '()
                       :node node)))
             ;; The scope used to be stored here as well as below. The tree
             ;; deduplicates the same object, so it was harmless -- but it also
             ;; meant the store happened before PARENT-SCOPE and
             ;; SYMBOL-DEFINITIONS were filled in, which reads as though order
             ;; does not matter. It does; the single store is at the end.
             ;; Make a symbol-definition for the function/macro name if applicable
             (when defun-name-n
                   (let* ((defun-name (fast-node-text defun-name-n source file-path))
                          (symbol-def (make-symbol-definition
                                        :symbol-name defun-name
                                        :package-name *current-package*
                                        :kind (defun-keyword-kind defun-type)
                                        :location (location-for-node file-path (first defun-header-children))
                                        ;; :defining-scope nil)))
                                        :defining-scope *current-scope*
                                        :node defun-name-n
                                        ;; The whole (defun ... ) form.
                                        :form-node node)))
                         ;; (slog :debug "Found ~A named: ~A" defun-type defun-name)
                         (push symbol-def (lexical-scope-symbol-definitions *current-scope*))
                         ;; Add top-level definitions to workspace index for cross-file lookup
                         (when (eq (lexical-scope-kind *current-scope*) :document)
                               (add-to-workspace-index symbol-def))))
             ;; Make a symbol-definition for each param
             (dolist (param param-nodes)
                     (let* ((param-name (fast-node-text param source file-path))
                            (symbol-def (make-symbol-definition
                                          :symbol-name param-name
                                          :package-name *current-package*
                                          :kind :variable
                                          :location (location-for-node file-path param)
                                          ;; :defining-scope nil)))
                                          :defining-scope scope
                                          :node param)))
                           ;; (slog :debug "defun-type = ~A, defun-name = ~A, param-name = ~A"
                           ;;       defun-type
                           ;;       (if defun-name-n
                           ;;           (node-text defun-name-n source)
                           ;;           "<lambda>")
                           ;;       param-name)
                           (push symbol-def defs)))
             ;; Update current scope for iterations down the tree after setting its parent and the collected defs
             (setf (lexical-scope-parent-scope scope) *current-scope*)
             ;; Store scope into the appropriate interval tree for fast lookup based in editor caret position
             (store-scope-on-interval-tree scope file-path)
             (setf (lexical-scope-symbol-definitions scope) (nreverse defs))
             (setf *current-scope* scope)
             scope))

(defun definition-visible-from-p (definition scope offset)
       "Is DEFINITION, which belongs to SCOPE, in scope at OFFSET?

The rule Common Lisp actually has, which one interval per form cannot express:

  - Past the end of the binding list -- in the body -- every binding is visible.
  - On the binding's own name, it is visible, so go-to-definition on a binding
    lands on itself rather than on whatever it shadows.
  - Inside the binding list otherwise, it depends on the form, and there are
    three answers rather than two. LET and FLET expose none of their own
    bindings there. LET* exposes the ones whose binding form has already closed,
    so a binding is visible in later init forms but not in its own. LABELS
    exposes all of them, forward references included -- that is precisely what
    makes mutual recursion legal in LABELS and not in FLET.

Without this, (let ((total (* total 2))) ...) resolved the init form's TOTAL to
the binding being established rather than to the outer one -- which made rename
emit edits that do not compile. See docs/surveys/lsp-review.md §3g."
       (let ((bindings-end (lexical-scope-bindings-end scope)))
            (cond
              ;; Not a form with a binding list, or we do not know where it ends.
              ((null bindings-end) t)
              ((null offset) t)
              ((>= offset bindings-end) t)
              (t
                (let* ((location (symbol-definition-location definition))
                       (start (when location (location-start location)))
                       (end (when location (location-end location))))
                     (cond
                       ((or (null start) (null end)) t)
                       ;; The cursor is on the binding's own name.
                       ((and (>= offset start) (< offset end)) t)
                       (t
                         (case (lexical-scope-binding-visibility scope)
                           ;; LABELS: everything sees everything, forward
                           ;; references included.
                           (:all t)
                           ;; LET*: visible once its own binding form is closed.
                           ;; Measured against the NAME would be wrong -- the
                           ;; name ends before its own init form begins, so the
                           ;; binding would be visible inside the very init form
                           ;; that establishes it.
                           (:preceding
                             (let* ((form (symbol-definition-form-node definition))
                                    (form-end (when form
                                                    (nth-value 1 (byte-offsets-for-node
                                                                   (location-file-path location)
                                                                   form)))))
                                   (if form-end (<= form-end offset) (<= end offset))))
                           ;; LET / FLET: none of this form's bindings.
                           (t nil)))))))))

(defun store-scope-on-interval-tree (scope file-path)
       "Stores the given lexical SCOPE into the interval tree for FILE-PATH.

**The tree keeps only the FIRST interval for any given pair of bounds.**
Measured, not assumed: inserting two intervals with identical start and end and
different data leaves one, holding the data of the first. The same object
inserted twice is likewise deduplicated, which is why CHECK-FOR-DEFUN storing
its scope twice was harmless.

That is why the document scope is made to span one past the end of the file --
see BUILD-FILE-SYMBOL-MAP. A file whose single top-level form covers the whole
text would otherwise produce a scope with exactly the document scope's bounds,
and vanish. The warning below exists because the general case is not solved: any
two scopes that happen to share bounds still collide, and silence is what made
this take so long to find."
       (let* ((scopes-tree (gethash file-path ctx:lexical-scopes))
              (start (location-start (lexical-scope-location scope)))
              (end (location-end (lexical-scope-location scope)))
              (existing (find-if (lambda (iv)
                                         (and (= (clef-interval-start iv) start)
                                              (= (clef-interval-end iv) end)
                                              (not (eq (clef-interval-data iv) scope))))
                                 (ignore-errors (interval:find-all scopes-tree (cons start end)))))
              (new-interval (make-clef-interval :start start :end end)))
             (when existing
                   (slog :warn "Scope ~A at [~A ~A] in ~A collides with an existing ~A scope and will be dropped."
                         (lexical-scope-kind scope) start end file-path
                         (lexical-scope-kind (clef-interval-data existing))))
             (setf (clef-interval-data new-interval) scope)
             (interval:insert scopes-tree new-interval)))

(defun check-for-local-function-binding (node node-type file-path source)
       "FLET, LABELS and MACROLET: local function names and their parameters.

Nothing handled these, so +SCOPE-KINDS+ advertised :flet and :labels while
nothing ever constructed either. A parameter shadowing an outer binding
therefore resolved to the outer one, because the inner binding did not exist:

    (let ((area (* radius radius)))
      (flet ((scale (area) (* area 2)))   ; a DIFFERENT binding named AREA
        ...))

Two levels of scope, because they have different extents. The local function
names belong to the whole form; each binding's parameters belong only to that
binding. See docs/surveys/lsp-review.md §1.2."
       (unless (equal node-type '(:value :list-lit))
               (return-from check-for-local-function-binding nil))
       (let* ((children (ts:node-children node))
              (head (first children))
              (kind (when (and head (equal (ts:node-type head) '(:value :sym-lit)))
                          (let ((text (fast-node-text head source file-path)))
                               (cond ((string-equal text "flet") :flet)
                                     ((string-equal text "labels") :labels)
                                     ;; MACROLET binds the same way; it defines
                                     ;; macros rather than functions, and clef
                                     ;; does not distinguish the two.
                                     ((string-equal text "macrolet") :flet)
                                     (t nil))))))
             (unless kind (return-from check-for-local-function-binding nil))
             (let* ((bindings-node (second children))
                    (bindings (ts:node-children bindings-node))
                    (scope (make-lexical-scope
                            :kind kind
                            ;; LABELS bindings see each other -- that is the
                            ;; whole difference from FLET, and what makes mutual
                            ;; recursion legal in one and not the other.
                            :bindings-end (nth-value 1 (byte-offsets-for-node file-path
                                                                              bindings-node))
                            :binding-visibility (if (eq kind :labels) :all :none)
                            :location (location-for-node file-path node)
                            :parent-scope *current-scope*
                            :symbol-definitions '()
                            :symbol-references (make-hash-table)
                            :child-scopes '()
                            :node node)))
                  (setf *current-scope* scope)
                  (store-scope-on-interval-tree scope file-path)
                  (dolist (binding bindings)
                          (when (equal (ts:node-type binding) '(:value :list-lit))
                                (let* ((parts (ts:node-children binding))
                                       (name-node (first parts))
                                       (lambda-list (second parts)))
                                      ;; The local function's own name, visible
                                      ;; throughout the form.
                                      (when (and name-node
                                                 (equal (ts:node-type name-node)
                                                        '(:value :sym-lit)))
                                            (push (make-symbol-definition
                                                    :symbol-name (fast-node-text name-node source file-path)
                                                    :package-name *current-package*
                                                    :kind :function
                                                    :location (location-for-node file-path name-node)
                                                    :defining-scope scope
                                                    :node name-node
                                                    :form-node binding)
                                                  (lexical-scope-symbol-definitions scope)))
                                      ;; Its parameters, in a scope covering only
                                      ;; this binding, which is what makes the
                                      ;; shadowing come out right.
                                      (when (and lambda-list
                                                 (equal (ts:node-type lambda-list)
                                                        '(:value :list-lit)))
                                            (let ((param-scope
                                                    (make-lexical-scope
                                                      :kind :lambda
                                                      :location (location-for-node file-path binding)
                                                      :parent-scope scope
                                                      :symbol-definitions '()
                                                      :symbol-references (make-hash-table)
                                                      :child-scopes '()
                                                      :node binding)))
                                                 (dolist (param (ts:node-children lambda-list))
                                                         (when (equal (ts:node-type param)
                                                                      '(:value :sym-lit))
                                                               (push (make-symbol-definition
                                                                       :symbol-name (fast-node-text param source file-path)
                                                                       :package-name *current-package*
                                                                       :kind :variable
                                                                       :location (location-for-node file-path param)
                                                                       :defining-scope param-scope
                                                                       :node param
                                                                       :form-node binding)
                                                                     (lexical-scope-symbol-definitions param-scope))))
                                                 (store-scope-on-interval-tree param-scope file-path))))))
                  scope)))

(defun check-for-let-binding (node node-type file-path source)
       "Check for 'let' or 'let*' bindings that create new lexical scopes and variable definitions."
       ;; A let or let* node in the AST is one where the node's type is (:value :list-lit),
       ;; its first-child has type (:value :sym-lit) with text being "let" or "let*",
       ;; and it's second-child is another (:value :list-lit) containing the bindings
       (when (not (equal node-type '(:value :list-lit)))
             (return-from check-for-let-binding nil))

       (let* ((children (ts:node-children node))
              (first-child (first children))
              (first-child-type (ts:node-type first-child)))
             (unless (and first-child
                          (equal first-child-type '(:value :sym-lit))
                          (let ((sym-text (fast-node-text first-child source file-path)))
                               (or (string= sym-text "let")
                                   (string= sym-text "let*"))))
                     (return-from check-for-let-binding nil)))

       ;; (slog :debug "getting let defines")
       (let* ((bindings-node (second (ts:node-children node)))
              (let-var-nodes (ts:node-children bindings-node))
              (visibility (let ((head (fast-node-text (first (ts:node-children node))
                                                      source file-path)))
                               (if (and head (string= head "let*")) :preceding :none)))
              (scope (make-lexical-scope
                      :kind :let
                      ;; Where the binding list ends, so a reference inside it
                      ;; can be told from one in the body.
                      :bindings-end (nth-value 1 (byte-offsets-for-node file-path
                                                                        bindings-node))
                      :binding-visibility visibility
                      :location (location-for-node file-path node)
                      :parent-scope *current-scope*
                      :symbol-definitions '()
                      :symbol-references (make-hash-table)
                      :child-scopes '()
                      :node node)))
            ;; Update current scope
            (setf *current-scope* scope)
            (store-scope-on-interval-tree scope file-path)
            ;; (slog :debug "Processing node: ~A" (node-text node source))
            ;; (slog :debug "Processing ~A let bindings" (length let-var-nodes))
            ;; TODO: I think there's a bug here as let can supposedly support a syntax like
            ;; 'let (alist)'
            (dolist (let-var-node let-var-nodes)
                    ;; (slog :debug "var-children are: ~ A" (ts:node-children let-var-node))
                    ;; (if (and (listp (ts:node-children let-var-node)) nil)
                    ;;     (slog :debug "var-children are ~A" (ts:node-children let-var-node))
                    ;;     (slog :debug "let-var-node is ~A" let-var-node))
                    (let* ((var-children (ts:node-children let-var-node))
                           ;; Note that (listp nil) is T in common lisp
                           (var-node (if (and (listp var-children)
                                              (not (null var-children)))
                                         (first var-children)
                                         let-var-node)))
                          (when (and var-node
                                     (equal (ts:node-type var-node) '(:value :sym-lit)))
                                (let* ((var-name (fast-node-text var-node source file-path))
                                       (symbol-def (make-symbol-definition
                                                     :symbol-name var-name
                                                     :package-name *current-package*
                                                     :kind :variable
                                                     :location (location-for-node file-path
                                                                                  var-node)
                                                     ;; :defining-scope nil)))
                                                     :defining-scope scope
                                                     :node var-node
                                                     ;; The whole binding pair,
                                                     ;; (total (* total 2)), not
                                                     ;; just the name. LET* needs
                                                     ;; it to keep a binding out
                                                     ;; of its own init form.
                                                     :form-node let-var-node)))
                                      ;; (slog :debug "var node is ~A" var-node)
                                      ;; (slog :debug "Found let binding named: ~A" var-name)
                                      ;; Add this def the let-binding scope
                                      (push symbol-def (lexical-scope-symbol-definitions *current-scope*))))))))


(defun check-for-simple-define (node node-type file-path source)
       "Check for 'simple' global var definitions like defparamater, defconstant, etc."
       ;; Ensure the node is a list-lit with a first child that's a (:value :sym-lit), where the sym-lit
       ;; is either 'defparameter', 'defconstant', or 'defvar'
       (when (not (eq node-type :LIST-LIT))
             (return-from check-for-simple-define nil))
       (let* ((children (ts:node-children node))
              (first-child (first children))
              (first-child-type (ts:node-type first-child)))
             (unless (and first-child
                          (equal first-child-type '(:value :sym-lit))
                          (let ((sym-text (fast-node-text first-child source file-path)))
                               ;; (slog :debug "sym-text: ~A" sym-text)
                               (or (string= sym-text "defparameter")
                                   (string= sym-text "defconstant")
                                   (string= sym-text "defvar"))))
                     (return-from check-for-simple-define nil)))
       ;; Create a new symbol-definition and add it to the current scope
       (let* ((children (ts:node-children node))
              ;; (first-child (first children))
              ;; (define-type (node-text first-child source))
              (name-node (second children))
              (var-name (fast-node-text name-node source file-path))
              (symbol-def (make-symbol-definition
                            :symbol-name var-name
                            :package-name *current-package*
                            :kind :variable
                            :location (location-for-node file-path name-node)
                            ;; :defining-scope nil)))
                            :defining-scope *current-scope*
                            :node name-node
                            ;; The whole (defparameter ... ) form.
                            :form-node node)))
             ;; (slog :debug "Found ~A named: ~A" define-type var-name)
             (push symbol-def (lexical-scope-symbol-definitions *current-scope*))
             ;; Add top-level definitions to workspace index for cross-file lookup
             ;; (defparameter/defvar/defconstant are always top-level)
             (when (eq (lexical-scope-kind *current-scope*) :document)
                   (add-to-workspace-index symbol-def))))

;;; ---------------------------------------------------------------------------
;;; Type-defining forms: DEFCLASS, DEFINE-CONDITION, DEFSTRUCT, DEFTYPE
;;;
;;; Before this, the index understood functions and variables and nothing else,
;;; which left every CLOS class, every structure, every condition and every type
;;; invisible to go-to-definition and to workspace symbol. In clef's own source
;;; that meant the whole of jsonrpc/types.lisp and the whole of
;;; lsp/types/base/error-codes.lisp -- and REQUEST-PARAMS, an accessor used
;;; throughout, resolved to nothing at all.
;;;
;;; All four arrive as a plain :LIST-LIT with a :SYM-LIT head, unlike DEFUN
;;; which the grammar gives a node of its own. Shapes measured in
;;; docs/experiments/lsp/02-type-form-shapes.lisp.
;;;
;;; The accessors a DEFSTRUCT generates are recorded too. They appear nowhere in
;;; the source text, so nothing else could ever find them -- and they are how
;;; structures are actually used.
;;; ---------------------------------------------------------------------------

(defun node-kind-of (node)
       "Tree-sitter node types arrive either bare (:LIST-LIT) or paired with a
field name ((:VALUE :SYM-LIT)). Normalise to the kind."
       (let ((type (ts:node-type node)))
            (if (consp type) (second type) type)))

(defun kind-is (node kind)
       (eq (node-kind-of node) kind))

(defun head-text (node source file-path)
       "Text of NODE's first child when that child is a symbol, else NIL."
       (let ((first-child (first (ts:node-children node))))
            (when (and first-child (kind-is first-child :sym-lit))
                  (fast-node-text first-child source file-path))))

(defun keyword-text-p (node text source file-path)
       "Is NODE the keyword named TEXT? Compares without the leading colon."
       (and (kind-is node :kwd-lit)
            (let ((raw (fast-node-text node source file-path)))
                  (and raw (string-equal (string-left-trim ":" raw) text)))))

(defun record-definition (name name-node kind file-path form-node
                          &optional (name-start-shift 0) (name-end-shift 0))
       "Record a global definition and add it to the workspace index.

NAME-START-SHIFT and NAME-END-SHIFT trim the recorded location inward from the
node's own extent. DEFPACKAGE names its package with a marker -- :foo, #:foo,
\"FOO\" -- which is not part of the name, so the name is normalised without it
and the location has to be trimmed to match. Otherwise the reported name and
the range covering it disagree, which is what the corpus sweep's
`selectionRange text /= name' invariant catches."
       (let* ((raw (location-for-node file-path name-node))
              (location (if (or (plusp name-start-shift) (plusp name-end-shift))
                            (make-location
                              :file-path (location-file-path raw)
                              :start (+ (location-start raw) name-start-shift)
                              :end (- (location-end raw) name-end-shift))
                            raw))
              (symbol-def (make-symbol-definition
                           :symbol-name name
                           :package-name *current-package*
                           :kind kind
                           :location location
                           :name-start-shift name-start-shift
                           :defining-scope *current-scope*
                           :node name-node
                           :form-node form-node)))
            (push symbol-def (lexical-scope-symbol-definitions *current-scope*))
            (when (eq (lexical-scope-kind *current-scope*) :document)
                  (add-to-workspace-index symbol-def))
            symbol-def))

(defun record-slot-accessors (slots-node file-path source form-node)
       "Record every :ACCESSOR, :READER and :WRITER named in a slot list.

Each is a generic function the form defines, and each is how the slot is
actually reached from other code -- REQUEST-PARAMS, LSP-ERROR-CODE and the rest
are all of this shape."
       (dolist (slot (ts:node-children slots-node))
               (when (kind-is slot :list-lit)
                     (let ((children (ts:node-children slot)))
                          (loop for (item next) on children
                                when (and next
                                          (kind-is next :sym-lit)
                                          (or (keyword-text-p item "accessor" source file-path)
                                              (keyword-text-p item "reader" source file-path)
                                              (keyword-text-p item "writer" source file-path)))
                                  do (record-definition (fast-node-text next source file-path)
                                                        next :function file-path form-node))))))

(defun check-for-class-define (node node-type file-path source)
       "DEFCLASS and DEFINE-CONDITION: the name, and every slot accessor.

    (defclass shape (base) ((name :initarg :name :accessor shape-name)))
             ^^^^^                                                ^^^^^^^^^^"
       (unless (eq node-type :list-lit)
               (return-from check-for-class-define nil))
       (let ((head (head-text node source file-path)))
            (unless (and head (or (string-equal head "defclass")
                                  (string-equal head "define-condition")))
                    (return-from check-for-class-define nil)))
       (let* ((children (ts:node-children node))
              (name-node (second children))
              (slots-node (fourth children)))
            (when (and name-node (kind-is name-node :sym-lit))
                  (record-definition (fast-node-text name-node source file-path)
                                     name-node :class file-path node))
            (when (and slots-node (kind-is slots-node :list-lit))
                  (record-slot-accessors slots-node file-path source node))))

(defun defstruct-name-node (spec)
       "The struct's name node, whether written bare or with options.

    (defstruct point ...)              -> the POINT symbol
    (defstruct (circle (:conc-name c-)) ...) -> the CIRCLE symbol"
       (cond ((null spec) nil)
             ((kind-is spec :sym-lit) spec)
             ((kind-is spec :list-lit)
              (let ((first-child (first (ts:node-children spec))))
                   (when (and first-child (kind-is first-child :sym-lit))
                         first-child)))
             (t nil)))

(defun defstruct-option-value (spec option source file-path)
       "The symbol given for OPTION in a DEFSTRUCT options list, or NIL.

Returns :NONE when the option is present but given no value -- (:conc-name)
and (:conc-name nil) both mean \"no prefix\", which is different from the option
being absent and the prefix defaulting to the struct name."
       (when (and spec (kind-is spec :list-lit))
             (dolist (child (rest (ts:node-children spec)))
                     (when (kind-is child :list-lit)
                           (let ((parts (ts:node-children child)))
                                (when (keyword-text-p (first parts) option source file-path)
                                      (let ((value (second parts)))
                                            (return
                                              (cond ((null value) :none)
                                                    ((kind-is value :sym-lit)
                                                     (let ((text (fast-node-text value source file-path)))
                                                          (if (string-equal text "nil")
                                                              :none
                                                              text)))
                                                    (t :none))))))))))

(defun slot-name-node (slot)
       "A struct slot is either a bare symbol or (name default ...)."
       (cond ((kind-is slot :sym-lit) slot)
             ((kind-is slot :list-lit)
              (let ((first-child (first (ts:node-children slot))))
                   (when (and first-child (kind-is first-child :sym-lit))
                         first-child)))
             (t nil)))

(defun check-for-struct-define (node node-type file-path source)
       "DEFSTRUCT: the type, its constructor, its predicate, and its accessors.

Everything but the type name is generated rather than written, so nothing that
searches the source text could ever find them -- and they are exactly how a
structure gets used. CLEF-CONDITIONS:DIAGNOSTIC-SEVERITY is one of these."
       (unless (eq node-type :list-lit)
               (return-from check-for-struct-define nil))
       (let ((head (head-text node source file-path)))
            (unless (and head (string-equal head "defstruct"))
                    (return-from check-for-struct-define nil)))
       (let* ((children (ts:node-children node))
              (spec (second children))
              (name-node (defstruct-name-node spec)))
            (unless name-node (return-from check-for-struct-define nil))
            (let* ((name (fast-node-text name-node source file-path))
                   (conc (defstruct-option-value spec "conc-name" source file-path))
                   (prefix (cond ((null conc) (concatenate 'string name "-"))
                                 ((eq conc :none) "")
                                 (t conc)))
                   (constructor (defstruct-option-value spec "constructor" source file-path)))
                  (record-definition name name-node :struct file-path node)
                  ;; The generated constructor and predicate. Both point at the
                  ;; struct name, which is the only place there is to point.
                  (unless (eq constructor :none)
                          (record-definition (if (stringp constructor)
                                                 constructor
                                                 (concatenate 'string "make-" name))
                                             name-node :function file-path node))
                  (record-definition (concatenate 'string name "-p")
                                     name-node :function file-path node)
                  (record-definition (concatenate 'string "copy-" name)
                                     name-node :function file-path node)
                  ;; One accessor per slot, pointing at the slot itself.
                  (dolist (slot (cddr children))
                          (let ((slot-node (slot-name-node slot)))
                                (when slot-node
                                      (record-definition
                                        (concatenate 'string prefix
                                                     (fast-node-text slot-node source file-path))
                                        slot-node :function file-path node)))))))

(defun check-for-type-define (node node-type file-path source)
       "DEFTYPE: the type name."
       (unless (eq node-type :list-lit)
               (return-from check-for-type-define nil))
       (let ((head (head-text node source file-path)))
            (unless (and head (string-equal head "deftype"))
                    (return-from check-for-type-define nil)))
       (let ((name-node (second (ts:node-children node))))
            (when (and name-node (kind-is name-node :sym-lit))
                  (record-definition (fast-node-text name-node source file-path)
                                     name-node :type file-path node))))

(defun check-for-symbol-reference (node node-type file-path source)
       "Checks if the given node is a symbol reference and records it in the current scope & file's
interval tree if so."
       ;; (:VALUE :SYM-LIT) is an ordinary symbol. (:SYMBOL :SYM-LIT) is the name
       ;; half of a package-qualified one, which the grammar gives its own shape:
       ;;
       ;;   (:VALUE :PACKAGE-LIT)   "clef-jsonrpc/types:request-params"
       ;;     (:PACKAGE :SYM-LIT)   "clef-jsonrpc/types"
       ;;     (:SYMBOL :SYM-LIT)    "request-params"
       ;;
       ;; Matching only (:VALUE :SYM-LIT) meant NO qualified use was ever entered
       ;; into the reference index -- so go-to-definition, find-references and
       ;; document-highlight all failed on them, which in a multi-package codebase
       ;; is most cross-package usage. See docs/surveys/lsp-review.md §3c.1.
       ;;
       ;; The package half is deliberately not recorded: it names a package, not
       ;; a symbol, and packages are not in the index.
       (unless (and (consp node-type)
                    (eq (second node-type) :sym-lit)
                    (member (first node-type) '(:value :symbol)))
               (return-from check-for-symbol-reference nil))
       ;; (slog :debug "Found (:value :sym-lit) node: ~A" (node-text node source))
       (let ((symbol-reference (make-symbol-reference
                                 :symbol-name (fast-node-text node source file-path)
                                 :package-name *current-package*
                                 :location (location-for-node file-path node)
                                 :usage-scope *current-scope*
                                 :node node)))
            (let ((refs-tree (gethash file-path ctx:symbol-refs))
                  (new-interval (make-clef-interval
                                  :start (location-start (symbol-reference-location symbol-reference))
                                  :end (location-end (symbol-reference-location symbol-reference)))))
                 (setf (clef-interval-data new-interval) symbol-reference)
                 (interval:insert refs-tree new-interval))
            ;; Also store into the current scope's symbol-references hash-table by appending the reference
            ;; to the end of the occurrences list for this symbol name
            (let ((scope-references-list (gethash (symbol-reference-symbol-name symbol-reference)
                                                  (lexical-scope-symbol-references *current-scope*))))
                 (if (not scope-references-list)
                     (setf scope-references-list '()))
                 (push symbol-reference scope-references-list))))
;; (slog :debug "Adding symbol-reference for ~A to current scope"
;;       (symbol-reference-symbol-name symbol-reference))
;; (slog :debug "New list is: ~A " scope-references-list))))

;;; ---------------------------------------------------------------------------
;;; DEFPACKAGE, and every other DEF* form nobody wrote a checker for
;;;
;;; Measured on clef's own 100 source files (docs/experiments/lsp/06-real-code-
;;; sweep.lisp): 645 of 828 definitions were indexed, and 87% of the misses were
;;; not ANSI gaps at all but DEFINING MACROS THE PROJECT DEFINES ITSELF --
;;; 144 DEFTEST, 16 DEFINE-CONTEXT-ACCESSOR. The hand-written corpus could never
;;; have shown that, because it was derived from the standard and so contains no
;;; project-specific macros.
;;;
;;; No amount of per-form checkers fixes that: any project can invent any DEF*
;;; macro. But the naming convention is near-universal in Common Lisp, and the
;;; standard follows it without exception -- all eighteen standard defining
;;; forms are DEF-prefixed with the name second. So treat that shape as a
;;; definition by default.
;;;
;;; Deliberately a heuristic, and it can be wrong. A macro named DEFER, or one
;;; whose second element is not the name it defines, produces a bogus entry. The
;;; trade is a wrong entry against a missing one, and for navigation a wrong
;;; entry is the cheaper error -- go-to-definition lands somewhere odd rather
;;; than nowhere, and the symbol at least appears in documentSymbol.
;;;
;;; This also picks up the four standard forms that had no checker:
;;; DEFINE-SETF-EXPANDER, DEFINE-MODIFY-MACRO, DEFINE-SYMBOL-MACRO and
;;; DEFINE-METHOD-COMBINATION -- see docs/surveys/cl-surface-area.md §9.2.

(defparameter *heads-with-their-own-checker*
              '("defun" "defmacro" "defmethod" "defgeneric" "lambda"
                "defparameter" "defconstant" "defvar"
                "defclass" "define-condition" "defstruct" "deftype")
              "DEF* heads handled by a dedicated checker.

Excluded here so a form is not recorded twice, once by its own checker and
once by the generic fallback.")

(defun define-form-name (text)
       "The symbol name TEXT designates, or NIL if it does not look like one.

DEFPACKAGE names its package as a keyword, a string or an uninterned symbol --
:clef-lsp/document, \"CLEF\", #:clef -- and all three mean the same name. Strip
the marker so the index holds one spelling.

Returns NIL for anything containing whitespace or a paren, which rejects a
list-shaped name such as (setf area) without having to inspect node types."
       (when (and text (plusp (length text)))
             (multiple-value-bind (name start-shift end-shift)
                 (cond
                   ((and (> (length text) 2) (string= "#:" (subseq text 0 2)))
                    (values (subseq text 2) 2 0))
                   ((char= (char text 0) #\:) (values (subseq text 1) 1 0))
                   ((char= (char text 0) #\") (values (string-trim "\"" text) 1 1))
                   (t (values text 0 0)))
               (when (and (plusp (length name))
                          (notany (lambda (c)
                                          (member c '(#\Space #\Tab #\Newline
                                                      #\( #\) #\' #\`)))
                                  name))
                     (values name start-shift end-shift)))))

(defun guessed-define-kind (head)
       "A plausible SYMBOL-KIND for an unrecognised DEF* form named HEAD.

A guess, and marked as one. The alternative is :UNKNOWN for every project macro,
which tells an editor nothing and shows no icon."
       (cond
         ((string-equal head "defpackage") :package)
         ((or (search "var" head) (search "global" head)
              (search "param" head) (search "constant" head))
          :variable)
         (t :function)))

(defun check-for-generic-define (node node-type file-path source)
       "Record any (DEF<something> NAME ...) form that no other checker claims."
       (when (not (eq node-type :LIST-LIT))
             (return-from check-for-generic-define nil))
       (let* ((children (ts:node-children node))
              (head-node (first children))
              (name-node (second children)))
            (unless (and head-node name-node
                         (equal (ts:node-type head-node) '(:value :sym-lit)))
                    (return-from check-for-generic-define nil))
            (let ((head (fast-node-text head-node source file-path)))
                 (unless (and head
                              (>= (length head) 4)
                              (string-equal "def" (subseq head 0 3))
                              (not (member head *heads-with-their-own-checker*
                                           :test #'string-equal)))
                         (return-from check-for-generic-define nil))
                 (multiple-value-bind (name start-shift end-shift)
                     (define-form-name (fast-node-text name-node source file-path))
                   (when name
                         (record-definition name name-node
                                            (guessed-define-kind head)
                                            file-path node
                                            start-shift end-shift))))))
