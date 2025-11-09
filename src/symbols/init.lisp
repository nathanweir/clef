(in-package :clef-symbols)

(defparameter *lexical-scopes-by-file* (make-hash-table)
              "A hash-table mapping file paths to interval trees of lexical-scope's")

(defparameter *symbol-refs-by-file* (make-hash-table)
              "A hash-table mapping file paths to interval trees of symbol-reference's")

(defparameter *current-scope* nil
              "The current lexical scope that is the context in which the current processed node is occurring")

;; TODO: This should be universal to the LSP and not specific to this package
(defparameter *document-line-lengths* (make-hash-table)
              "A hash-table mapping file paths to vectors of line lengths for that document.")

;; TODO: It is VERY annoying (and inefficient) to have to do this, but I've been unable to pull the byte
;; offsets out of death:cl-tree-sitter's low level API as they don't seem to be exposed in any way by
;; the nodes the high-level API creates
(defun line-char-to-byte-offset (file-path line char)
       "Converts a line and character position to a byte offset."
       (let* ((line-lengths (gethash file-path *document-line-lengths*))
              (line-index (1- line)) ;; Convert to 0-based
              (char-index char)     ;; Already 0-based
              (byte-offset 0))
             ;; Sum up all prior line lengths + newlines
             (dotimes (i line-index)
                      (incf byte-offset (aref line-lengths i))
                      (incf byte-offset 1)) ;; For newline
             ;; Add the char offset on the target line
             (incf byte-offset char-index)
             byte-offset))

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

(defun calculate-line-lengths (file-source)
       "Calculates the lengths of each line in the given file source."
       (let ((lines (cl-ppcre:split #\Newline file-source))
             (lengths '()))
            (dolist (line lines)
                    (push (length line) lengths))
            (concatenate 'vector (nreverse lengths))))

(defun filter-files (file-paths)
       "Filters out files from a list of paths to .lisp files that fit certain criteria"
       ;; Exclude any files under '.direnv'. Long-term we'd achieve this by processing the
       ;; .gitignore
       (remove-if-not (lambda (path)
                              ;; Limit to only util.lisp for now
                              (cl-ppcre:scan "/home/nathan/dev/clef/src/util\\.lisp" (namestring path)))
                      ;; (cl-ppcre:scan "\\.direnv" (namestring path)))
                      file-paths))

(defun build-symbol-map (project-root)
       (slog :debug "Building symbol map at ~A" project-root)
       ;; Discover every .lisp file recursively under the root
       (let* ((wildcard-path (concatenate 'string
                                          (clef-util:cleanup-path project-root)
                                          "/**/*.lisp"))
              (lisp-files (uiop:directory* wildcard-path))
              (filtered-files (filter-files lisp-files)))
             (slog :debug "Found ~A valid Lisp files in workspace." (length filtered-files))
             ;; Process each file to extract symbols
             (dolist (file-path filtered-files)
                     (let ((file-source (clef-util:read-file-text (namestring file-path))))
                          (process-file (namestring file-path) file-source)))))

(defparameter *package-name* nil "The name of the current package encountered when processing the file")

(defun process-file (file-path file-source)
       (slog :debug "Processing file for symbol-map: ~A" file-path)

       ;; Reset any previously found package names
       (setf *package-name* nil)

       ;; Calculate and store line lengths for this document
       (setf (gethash file-path *document-line-lengths*)
             (calculate-line-lengths file-source))

       ;; Init the interval trees
       (setf (gethash file-path *symbol-refs-by-file*)
             (interval:make-tree))
       (setf (gethash file-path *lexical-scopes-by-file*)
             (interval:make-tree))

       ;; Parse the file with tree-sitter and then walk the output tree to find
       ;; the current package, record symbol definitions, symbol references, and
       ;; lexical scopes
       (let ((parse-tree (clef-parser/parser:parse-string file-source)))
            ;; Create the initial lexical-scope
            (setf *current-scope*
                  (make-lexical-scope
                    :kind :document
                    :location (make-location
                                :file-path file-path
                                :start 0
                                :end (length file-source))
                    :parent-scope nil
                    :symbol-definitions '()
                    :symbol-references (make-hash-table)
                    :child-scopes '()))
            (labels ((walk (n)
                           (let ((type (ts:node-type n)))
                                (when (or (eql type :error) (eql type :missing))
                                      ;; TODO: What to do on syntax errors? Just abort?
                                      '())
                                ;; (push (cons type (ts:node-range n)) results))
                                (progn
                                  ;; (slog :debug "build-symbol-map> node-type is: ~A" type)
                                  ;; Update current tracked package by looking for (in-package package-name)
                                  (check-for-in-package n file-source file-path)
                                  ;; Look for forms that define new variables, thus creating a new lexical-scope
                                  ;; and relevant symbol-definition's
                                  (check-for-defun n file-path file-source)
                                  ;; Check if the node is a symbol-reference and record it into the scope if so
                                  ;; (uses *current-scope* internally intead of passing a scope in here)
                                  ;; (check-for-symbol-reference n file-path file-source)
                                  (dolist (child (ts:node-children n))
                                          (walk child))))))
                    (walk parse-tree))
            '())
       (slog :debug "package name found was: ~A" *package-name*))

(defun node-obj (node source)
       (let* ((text (clef-parser/parser:node-text node source))
              (object (read-from-string text)))
             object))

(defun check-for-in-package (node source file-path)
       "Checks if the given node is an in-package declaration and updates *package-name* if so"
       (declare (ignore file-path))
       ;; For debug, print start and end byte offsets for this node

       ;; (multiple-value-bind (start end) (byte-offsets-for-node file-path node)
       ;;                      (slog :debug "node byte offsets: ~A ~A" start end))
       ;; (slog :debug "node byte offsets: ~A ~A" (byte-offsets-for-node file-path node))

       (let ((text (clef-parser/parser:node-text node source)))
            ;; (slog :debug "text is: ~A" text)
            ;; Look for (in-package ...) forms
            (when (and (eq (ts:node-type node) :LIST-LIT)
                       (search "in-package" text))
                  (handler-case
                    (let ((form (read-from-string text)))
                         (when (and (consp form)
                                    (eq (car form) 'in-package))
                               (setf *package-name* (second form))))
                    (error () nil)))))

;; TODO: Check for the following types of definition nodes:
;; DEFUN, DEFMACRO, DEFPARAMETER, DESTRUCTRING-BIND
;; Also, LET, LET*, FLET
;; Others to consider in the future (?): DEFTYPE, DEFSPECIAL, DEFSTRUCT, DEFMETHOD, DEFCLASS
;; 'defun', 'defmacro', 'defgeneric', 'defmethod'

;; 'defun' is a bad name, but IIRC the parser calls at least some of these nodes "defun" nodes, even for
;; defmacro and lambdas
(defun check-for-defun (node file-path source)
       "If a 'defun' node is found, unpack the specific type of node, name, and params into
symbol-definitions. Returns the created lexical-scope if applicable, nil otherwise."
       (when (not (eq (ts:node-type node) :defun))
             (return-from check-for-defun nil))

       (let* ((defun-header-n (first (ts:node-children node)))
              (defun-header-children (ts:node-children defun-header-n))
              (defun-type (node-text (first defun-header-children) source))
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
                       :child-scopes '())))
             ;; (slog :debug "made scope for ~A at ~A" defun-type (location-for-node file-path node))
             (when defun-name-n
                   (let* ((defun-name (node-text defun-name-n source))
                          (symbol-def (make-symbol-definition
                                        :symbol-name defun-name
                                        :package-name *package-name*
                                        ;; TODO: Calc specific kind
                                        :kind :function
                                        :location (location-for-node file-path (first defun-header-children))
                                        :defining-scope scope)))
                         (slog :debug "Found ~A named: ~A" defun-type defun-name)
                         (push symbol-def defs)))
             ;; Make a symbol-definition for each param
             (dolist (param param-nodes)
                     (let* ((param-name (node-text param source))
                            (symbol-def (make-symbol-definition
                                          :symbol-name param-name
                                          :package-name *package-name*
                                          :kind :variable
                                          :location (location-for-node file-path param)
                                          :defining-scope scope)))
                           (slog :debug "defun-type = ~A, defun-name = ~A, param-name = ~A"
                                 defun-type
                                 (if defun-name-n
                                     (node-text defun-name-n source)
                                     "<lambda>")
                                 param-name)
                           (push symbol-def defs)))
             ;; Update current scope for iterations down the tree after setting its parent and the collected defs
             (setf (lexical-scope-parent-scope scope) *current-scope*)
             (setf (lexical-scope-symbol-definitions scope) (nreverse defs))
             (setf *current-scope* scope)
             scope))
