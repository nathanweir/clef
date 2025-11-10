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
(defparameter *document-line-offsets* (make-hash-table)
              "A hash-table mapping file paths to vectors of line offsets (starting byte offset) for that document.")

(defparameter *current-package* nil "The name of the current package encountered when processing the file")

(defun get-ref-for-doc-pos (file-path line char)
       "Gets the symbol reference name & lexical-scope for the given document position.
Note that symbol-ref can be nil if none is at the location"
       ;; (slog :debug ">>>>>>>>: ~A ~A ~A" file-path line char)
       (let* ((path (clef-util:cleanup-path file-path))
              (offset (line-char-to-byte-offset path line char))
              (symbol-refs (interval:find-all (gethash path *symbol-refs-by-file*) offset))
              ;; Also get the lexical scope by position, as symbol-refs may be nil
              (scopes (interval:find-all (gethash path *lexical-scopes-by-file*) offset)))
             ;; (slog :debug "Found symbol-defs at line ~A char ~A (offset ~A): ~A" line char offset symbol-defs)
             ;; (slog :debug ">>>> scope intervals found: ~A" scopes)
             ;; (values nil nil)))
             (values
               (when (and symbol-refs (consp symbol-refs))
                     (symbol-reference-symbol-name (clef-interval-data (first symbol-refs))))
               (clef-interval-data (first (last scopes))))))
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
       (let* ((line-offsets (gethash file-path *document-line-offsets*))
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
       (let* ((line-offsets (gethash file-path *document-line-offsets*))
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

(defun filter-files (file-paths)
       "Filters out files from a list of paths to .lisp files that fit certain criteria"
       ;; Exclude any files under '.direnv'. Long-term we'd achieve this by processing the
       ;; .gitignore
       (remove-if (lambda (path)
                          ;; Re-enable for testing to Limit to only one file
                          ;; (cl-ppcre:scan "/home/nathan/dev/clef/src/symbols/init\\.lisp" (namestring path)))
                          ;; (cl-ppcre:scan "/home/nathan/dev/clef/src/symbols/init\\.lisp" (namestring path)))
                          (cl-ppcre:scan "\\.direnv" (namestring path)))
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
       (setf (gethash file-path *document-line-offsets*)
             (calculate-line-offsets file-source))

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
                                  (check-for-simple-define n node-type file-path file-source)
                                  ;; Check if the node is a symbol-reference and record it into the scope if so
                                  ;; (uses *current-scope* internally intead of passing a scope in here)
                                  (check-for-symbol-reference n node-type file-path file-source)

                                  (dolist (child (ts:node-children n))
                                          (walk child))
                                  ;; Restore previous scope
                                  (setf *current-scope* previous-scope)))))
                    (walk parse-tree))
            '()))

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
             ;; (slog :debug "made scope for ~A at ~A" defun-type (location-for-node file-path node))
             (store-scope-on-interval-tree scope file-path)
             ;; Make a symbol-definition for the function/macro name if applicable
             (when defun-name-n
                   (let* ((defun-name (fast-node-text defun-name-n source file-path))
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

(defun store-scope-on-interval-tree (scope file-path)
       "Stores the given lexical SCOPE into the interval tree for FILE-PATH."
       (let ((scopes-tree (gethash file-path *lexical-scopes-by-file*))
             (new-interval (make-clef-interval
                             :start (location-start (lexical-scope-location scope))
                             :end (location-end (lexical-scope-location scope)))))
            (setf (clef-interval-data new-interval) scope)
            (interval:insert scopes-tree new-interval)))

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
                                                     :node var-node)))
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
                            :node name-node)))
             ;; (slog :debug "Found ~A named: ~A" define-type var-name)
             (push symbol-def (lexical-scope-symbol-definitions *current-scope*))))

(defun check-for-symbol-reference (node node-type file-path source)
       "Checks if the given node is a symbol reference and records it in the current scope & file's
interval tree if so."
       (unless (equal node-type '(:value :sym-lit))
               (return-from check-for-symbol-reference nil))
       ;; (slog :debug "Found (:value :sym-lit) node: ~A" (node-text node source))
       (let ((symbol-reference (make-symbol-reference
                                 :symbol-name (fast-node-text node source file-path)
                                 ;; :package-name *current-package* ;; TODO: revisit
                                 :location (location-for-node file-path node)
                                 :usage-scope *current-scope*
                                 :node node)))
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
