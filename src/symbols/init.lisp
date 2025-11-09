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

(defparameter *current-package* nil "The name of the current package encountered when processing the file")

(defun get-ref-for-doc-pos (file-path line char)
       "Gets the symbol reference name & lexical-scope for the given document position."
       ;; (slog :debug ">>>>>>>>: ~A ~A ~A" file-path line char)
       (let* ((path (clef-util:cleanup-path file-path))
              (offset (line-char-to-byte-offset path line char))
              (tree (gethash path *symbol-refs-by-file*))
              (symbol-refs (interval:find-all tree offset)))
             ;; (slog :debug "Found symbol-defs at line ~A char ~A (offset ~A): ~A" line char offset symbol-defs)
             ;; (slog :debug "symbol-name is: ~A"
             ;; (symbol-reference-symbol-name
             ;;   (clef-interval-data (first symbol-refs))))
             (if (> (length symbol-refs) 0)
                 (let ((symbol-ref (clef-interval-data (first symbol-refs))))
                      (values (symbol-reference-usage-scope symbol-ref)
                              (symbol-reference-symbol-name symbol-ref)))
                 (values nil nil))))

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
                              ;; Re-enable for testing to Limit to only one file
                              ;; (cl-ppcre:scan "/home/nathan/dev/clef/src/symbols/init\\.lisp" (namestring path)))
                              (cl-ppcre:scan "/home/nathan/dev/clef/src/util\\.lisp" (namestring path)))
                      ;; (cl-ppcre:scan "\\.direnv" (namestring path)))
                      file-paths))

(defun build-project-symbol-map (project-root)
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
                          (build-file-symbol-map (namestring file-path) file-source)))))

(defun build-file-symbol-map (file-path file-source)
       (slog :debug "Processing file for symbol-map: ~A" file-path)

       ;; Reset any previously found package names
       (setf *current-package* nil)

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
                    :child-scopes '()
                    :node parse-tree))
            (labels ((walk (n)
                           (let ((previous-scope *current-scope*)
                                 (type (ts:node-type n)))
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
                                  (check-for-let-binding n file-path file-source)
                                  (check-for-simple-define n file-path file-source)
                                  ;; Check if the node is a symbol-reference and record it into the scope if so
                                  ;; (uses *current-scope* internally intead of passing a scope in here)
                                  (check-for-symbol-reference n file-path file-source)

                                  (dolist (child (ts:node-children n))
                                          (walk child))
                                  ;; Restore previous scope
                                  (setf *current-scope* previous-scope)))))
                    (walk parse-tree))
            '())
       (slog :debug "package name found was: ~A" *current-package*))

(defun node-obj (node source)
       (let* ((text (clef-parser/parser:node-text node source))
              (object (read-from-string text)))
             object))

(defun check-for-in-package (node source file-path)
       "Checks if the given node is an in-package declaration and updates *current-package* if so"
       (declare (ignore file-path))
       ;; For debug, print start and end byte offsets for this node

       ;; (multiple-value-bind (start end) (byte-offsets-for-node file-path node)
       ;;                      (slog :debug "node byte offsets: ~A ~A" start end))
       ;; (slog :debug "node byte offsets: ~A ~A" (byte-offsets-for-node file-path node))

       (let ((text (node-text node source)))
            ;; (slog :debug "text is: ~A" text)
            ;; Look for (in-package ...) forms
            (when (and (eq (ts:node-type node) :LIST-LIT)
                       (search "in-package" text))
                  (handler-case
                    (let ((form (read-from-string text)))
                         (when (and (consp form)
                                    (eq (car form) 'in-package))
                               (setf *current-package* (second form))))
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
                       :child-scopes '()
                       :node node)))
             ;; (slog :debug "made scope for ~A at ~A" defun-type (location-for-node file-path node))
             (store-scope-on-interval-tree scope file-path)
             ;; Make a symbol-definition for the function/macro name if applicable
             (when defun-name-n
                   (let* ((defun-name (node-text defun-name-n source))
                          (symbol-def (make-symbol-definition
                                        :symbol-name defun-name
                                        :package-name *current-package*
                                        ;; TODO: Calc specific kind
                                        :kind :function
                                        :location (location-for-node file-path (first defun-header-children))
                                        ;; :defining-scope nil)))
                                        :defining-scope *current-scope*
                                        :node defun-name-n)))
                         ;; (slog :debug "Found ~A named: ~A" defun-type defun-name)
                         (push symbol-def (lexical-scope-symbol-definitions *current-scope*))))
             ;; Make a symbol-definition for each param
             (dolist (param param-nodes)
                     (let* ((param-name (node-text param source))
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

(defun store-scope-on-interval-tree (scope file-path)
       "Stores the given lexical SCOPE into the interval tree for FILE-PATH."
       (let ((scopes-tree (gethash file-path *lexical-scopes-by-file*))
             (new-interval (make-clef-interval
                             :start (location-start (lexical-scope-location scope))
                             :end (location-end (lexical-scope-location scope)))))
            (setf (clef-interval-data new-interval) scope)
            (interval:insert scopes-tree new-interval)))

(defun check-for-let-binding (node file-path source)
       "Check for 'let' or 'let*' bindings that create new lexical scopes and variable definitions."
       ;; A let or let* node in the AST is one where the node's type is (:value :list-lit),
       ;; its first-child has type (:value :sym-lit) with text being "let" or "let*",
       ;; and it's second-child is another (:value :list-lit) containing the bindings
       (when (not (equal (ts:node-type node) '(:value :list-lit)))
             (return-from check-for-let-binding nil))

       (let* ((children (ts:node-children node))
              (first-child (first children))
              (first-child-type (ts:node-type first-child)))
             (unless (and first-child
                          (equal first-child-type '(:value :sym-lit))
                          (let ((sym-text (node-text first-child source)))
                               (or (string= sym-text "let")
                                   (string= sym-text "let*"))))
                     (return-from check-for-let-binding nil)))

       ;; (slog :debug "getting let defines")
       (let ((let-var-nodes (ts:node-children (second (ts:node-children node))))
             (scope (make-lexical-scope
                      :kind :let
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
                    (let* ((var-node (first (ts:node-children let-var-node)))
                           (var-name (node-text
                                       var-node
                                       source))
                           (symbol-def (make-symbol-definition
                                         :symbol-name var-name
                                         :package-name *current-package*
                                         :kind :variable
                                         :location (location-for-node file-path
                                                                      var-node)
                                         ;; :defining-scope nil)))
                                         :defining-scope scope
                                         :node var-node)))
                          ;; (slog :debug "Found let binding named: ~A" var-name)
                          ;; Add this def the let-binding scope
                          (push symbol-def (lexical-scope-symbol-definitions *current-scope*))))))


(defun check-for-simple-define (node file-path source)
       "Check for 'simple' global var definitions like defparamater, defconstant, etc."
       ;; Ensure the node is a list-lit with a first child that's a (:value :sym-lit), where the sym-lit
       ;; is either 'defparameter', 'defconstant', or 'defvar'
       (when (not (eq (ts:node-type node) :LIST-LIT))
             (return-from check-for-simple-define nil))
       (let* ((children (ts:node-children node))
              (first-child (first children))
              (first-child-type (ts:node-type first-child)))
             (unless (and first-child
                          (equal first-child-type '(:value :sym-lit))
                          (let ((sym-text (node-text first-child source)))
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
              (var-name (node-text name-node source))
              (symbol-def (make-symbol-definition
                            :symbol-name var-name
                            :package-name *current-package*
                            :kind :variable
                            :location (location-for-node file-path name-node)
                            ;; :defining-scope nil)))
                            :defining-scope *current-scope*
                            :node name-node)))
             ;; (slog :debug "Found ~A named: ~A" define-type var-name)
             (push symbol-def (lexical-scope-symbol-definitions *current-scope*))))

(defun check-for-symbol-reference (node file-path source)
       "Checks if the given node is a symbol reference and records it in the current scope & file's
interval tree if so."
       (when (not (equal (ts:node-type node) '(:value :sym-lit)))
             (return-from check-for-symbol-reference nil))
       ;; (slog :debug "Found (:value :sym-lit) node: ~A" (node-text node source))
       (let ((symbol-reference (make-symbol-reference
                                 :symbol-name (node-text node source)
                                 ;; :package-name *current-package* ;; TODO: revisit
                                 :location (location-for-node file-path node)
                                 :usage-scope *current-scope*
                                 :node node)))
            ;; (slog :debug "Recording symbol-reference for: ~A" (symbol-reference-symbol-name symbol-reference))
            ;; Store symbol-reference into the appropriate interval tree for fast lookup based in editor caret position
            (let ((refs-tree (gethash file-path *symbol-refs-by-file*))
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
