(in-package :clef-lsp/lifecycle)

;;; Workspace ASDF system discovery and loading.
;;;
;;; The loaded-systems / file-to-system / asd-files tables formerly defined
;;; here as defparameters now live on CLEF-CONTEXT:*SERVER* so that all
;;; CLEF state resets atomically on shutdown. This file just reads and
;;; writes them through the CTX: aliases.

;; TODO: I need to move all of the asd/system loading into a thread for resiliency

(defun systems-in-asd (asd-uri)
       ;; For now, actually just assume the correct name is the symbol of the file name
       (let* ((system-name (pathname-name asd-uri)))
             (list (make-symbol system-name))))
;; TODO: use the tree sitter parser to find these. For now, read the file contents from
;; disk, regex out the first system, and then return the name as a list of one item
;; (let* ((file-contents (with-open-file (stream asd-uri)
;;                                       (let ((contents ""))
;;                                            (loop for line = (read-line stream nil)
;;                                                  while line
;;                                                  do (setf contents (concatenate 'string contents line "\n")))
;;                                            contents)))
;;        (system-name (cl-ppcre:scan-to-strings
;;                       "\\(defsystem\\s(.*)" file-contents)))
;;       ;; Return list of system names found
;;       (slog :debug "Found system names in ~A: ~A" asd-uri (first system-name))
;;       (if system-name
;;           (list (second (first system-name)))  ; Extract the first match's group
;;           nil)))

;; Probably not necessary; I haven't noticed these do anything
;; Set these globally or locally before loading
(setf asdf:*compile-file-failure-behaviour* :error)  ; Treat compilation failures as errors
(setf asdf:*compile-file-warnings-behaviour* :warn)  ; Keep warnings as warnings

(defun safe-load-system (system-name)
       "Load system with forced non-interactive error handling."
       (let ((sb-ext:*invoke-debugger-hook*
               (lambda (condition hook)
                       (declare (ignore hook))
                       (format *error-output* "Error: ~A~%" condition)
                       (format *error-output* "Condition type: ~A~%" (type-of condition))
                       ;; Exit the entire debugger context immediately
                       (sb-ext:exit :code 1))))
            (handler-case
              (progn
                (slog :debug ">>> would load system here: ~A" system-name)
                (asdf:load-system system-name))
              (error (c)
                     (format *error-output* "Load failed: ~A~%" c)
                     nil))))


(defun load-asd (asd-file)
       (slog :info "Loading .asd ~A" asd-file)
       (asdf:load-asd asd-file)
       (slog :debug "Loaded!" asd-file)
       ;; Load all the systems in the .asd
       (let ((system-names (systems-in-asd asd-file)))
            (dolist (system-name system-names)
                    ;; Skip the "clef" system as that's the name of this LSP and loading it will error
                    (if (string= (string-downcase system-name) "clef")
                        (slog :debug "Skipping system 'clef'")
                        (handler-case
                          (progn
                            ;; Make sure ASDF can find the system
                            (pushnew (directory-namestring asd-file)
                                     asdf:*central-registry*
                                     :test #'equal)
                            ;; Load the system
                            (slog :debug "Loading ASDF system ~A from file ~A..." system-name asd-file)
                            (if (asdf:find-system system-name nil)
                                (progn
                                  (slog :debug "System exists, will load: ~A" system-name)
                                  (safe-load-system system-name)
                                  (slog :debug "Loaded system!"))
                                (slog :debug "Could not find system to load: ~A" system-name)))
                          (error (e)
                                 ;; Log but don't fail if system can't be loaded
                                 (slog :debug "Warning: Could not load system ~A: ~A"
                                       system-name e)))))))

;; TODO: This is currently exported and used in
;; textDocument/didSave, which is weird organization
(defun load-workspace-asd (root-uri)
       "Finds the first .asd file in the workspace root uri, and loads it"
       ;; TODO: Handle missing trailing slash
       (let* ((path-root (clef-util:cleanup-path root-uri))
              (wildcard-path (concatenate 'string path-root "/" "*.asd"))
              (asd-files (uiop:directory* wildcard-path)))
             (if asd-files
                 (load-asd (first asd-files))
                 ;; TODO: Throw an error? Need to decide on how to handle cases where we can't
                 ;; load .asd
                 (slog :debug "No .asd files found in workspace root: ~A" wildcard-path))))

;;; ============================================================================
;;; Multi-ASD File Support
;;; ============================================================================

(defun discover-asd-files (root-uri)
  "Find all .asd files in the workspace: root + common subdirectories (test/, tests/, t/)."
  (let* ((path-root (clef-util:cleanup-path root-uri))
         (search-dirs (list path-root
                            (concatenate 'string path-root "/test")
                            (concatenate 'string path-root "/tests")
                            (concatenate 'string path-root "/t")))
         (all-asd-files '()))
    ;; Search each directory that exists
    (dolist (dir search-dirs)
      (when (uiop:directory-exists-p dir)
        (let* ((wildcard-path (concatenate 'string dir "/*.asd"))
               (found-files (uiop:directory* wildcard-path)))
          (setf all-asd-files (append all-asd-files found-files)))))
    ;; Remove duplicates (in case of symlinks) and return
    (remove-duplicates all-asd-files :test #'equal)))

(defun get-node-text-simple (node source)
  "Extract text for a tree-sitter node without requiring line offset caching."
  (let* ((start-row (clef-parser/parser:node-start-point-row node))
         (start-col (clef-parser/parser:node-start-point-column node))
         (end-row (clef-parser/parser:node-end-point-row node))
         (end-col (clef-parser/parser:node-end-point-column node)))
    ;; Calculate byte offsets by counting through newlines
    (let ((start-offset 0)
          (end-offset 0)
          (current-row 0)
          (current-col 0))
      ;; Find start offset
      (loop for i from 0 below (length source)
            do (when (and (= current-row start-row)
                          (= current-col start-col))
                 (setf start-offset i)
                 (return))
               (if (char= (char source i) #\Newline)
                   (progn (incf current-row) (setf current-col 0))
                   (incf current-col)))
      ;; Continue to find end offset
      (setf current-row 0 current-col 0)
      (loop for i from 0 below (length source)
            do (when (and (= current-row end-row)
                          (= current-col end-col))
                 (setf end-offset i)
                 (return))
               (if (char= (char source i) #\Newline)
                   (progn (incf current-row) (setf current-col 0))
                   (incf current-col)))
      (subseq source start-offset (min end-offset (length source))))))

(defun parse-asd-file (asd-path)
  "Parse an .asd file and return a list of system-info structs for each defsystem found."
  (handler-case
      (let* ((source (clef-util:read-file-text (namestring asd-path)))
             (tree (clef-parser/parser:parse-string source))
             (systems '()))
        ;; Walk tree looking for defsystem forms
        (labels ((walk (node)
                   (let ((node-type (cl-tree-sitter/high-level:node-type node)))
                     ;; Look for list literals that might be defsystem
                     (when (eq node-type :LIST-LIT)
                       (let ((parsed-system (try-parse-defsystem node source)))
                         (when parsed-system
                           (setf (clef-symbols:system-info-asd-path parsed-system)
                                 (namestring asd-path))
                           (push parsed-system systems))))
                     ;; Recurse into children
                     (dolist (child (cl-tree-sitter/high-level:node-children node))
                       (walk child)))))
          (walk tree))
        (nreverse systems))
    (error (e)
      (slog :warn "Failed to parse .asd file ~A: ~A" asd-path e)
      nil)))

(defun try-parse-defsystem (list-node source)
  "Attempt to parse a LIST-LIT node as a defsystem form. Returns system-info or nil."
  (let* ((children (cl-tree-sitter/high-level:node-children list-node))
         (first-child (first children)))
    ;; Check if first child is 'defsystem' or 'asdf:defsystem' symbol
    (when (and first-child
               (let ((first-type (cl-tree-sitter/high-level:node-type first-child)))
                 (or (equal first-type '(:value :sym-lit))
                     (equal first-type '(:value :kwd-lit)))))
      (let ((first-text (string-downcase (get-node-text-simple first-child source))))
        (when (or (string= first-text "defsystem")
                  (string= first-text "asdf:defsystem"))
          ;; Second child should be the system name
          (let ((name-node (second children)))
            (when name-node
              (let* ((name-text (get-node-text-simple name-node source))
                     ;; Clean up the name (remove quotes/colons, handle keywords)
                     (system-name (string-downcase
                                   (string-trim '(#\: #\" #\' #\#) name-text)))
                     (dependencies (extract-depends-on children source)))
                (clef-symbols:make-system-info
                 :name system-name
                 :asd-path nil  ; Set by caller
                 :dependencies dependencies
                 :source-files nil  ; Populated after loading
                 :loaded-p nil)))))))))

(defun extract-depends-on (children source)
  "Extract the :depends-on list from defsystem children nodes."
  (let ((found-depends-on nil))
    (dolist (child children)
      (let ((child-type (cl-tree-sitter/high-level:node-type child)))
        (cond
          ;; If we previously found :depends-on, the next list is our deps
          ((and found-depends-on
                (or (equal child-type '(:value :list-lit))
                    (eq child-type :LIST-LIT)))
           (handler-case
               (let ((deps-text (get-node-text-simple child source)))
                 (return-from extract-depends-on (read-from-string deps-text)))
             (error () nil)))
          ;; Look for :depends-on keyword
          ((and (or (equal child-type '(:value :kwd-lit))
                    (eq child-type :KWD-LIT))
                (string-equal (get-node-text-simple child source) ":depends-on"))
           (setf found-depends-on t)))))
    nil))

(defun get-system-source-files (system-name)
  "Get all source files belonging to an ASDF system after it's loaded."
  (let ((system (asdf:find-system system-name nil)))
    (when system
      (let ((files '()))
        ;; Walk through all components and collect source files
        (labels ((collect-from-component (component)
                   (typecase component
                     (asdf:source-file
                      (let ((pathname (asdf:component-pathname component)))
                        (when pathname
                          (push (namestring pathname) files))))
                     (asdf:module
                      (dolist (child (asdf:component-children component))
                        (collect-from-component child))))))
          (collect-from-component system))
        (nreverse files)))))

(defun compute-system-load-order ()
  "Compute topological order for loading systems based on inter-project dependencies.
Systems with no local dependencies are loaded first."
  (let* ((systems ctx:loaded-systems)
         (local-system-names (loop for name being the hash-keys of systems
                                   collect name))
         (no-local-deps '())
         (with-local-deps '()))
    ;; Partition systems by whether they have local dependencies
    (maphash (lambda (name sys-info)
               (let* ((deps (clef-symbols:system-info-dependencies sys-info))
                      ;; Convert deps to strings for comparison
                      (deps-as-strings (mapcar (lambda (d)
                                                 (string-downcase (if (stringp d) d (symbol-name d))))
                                               deps))
                      (local-deps (intersection deps-as-strings local-system-names
                                                :test #'string-equal)))
                 (if (null local-deps)
                     (push name no-local-deps)
                     (push name with-local-deps))))
             systems)
    ;; Return systems without local deps first, then those with deps
    (append (nreverse no-local-deps) (nreverse with-local-deps))))

(defun load-system-with-info (sys-info)
  "Load a single system using its system-info struct."
  (let* ((asd-path (clef-symbols:system-info-asd-path sys-info))
         (system-name (clef-symbols:system-info-name sys-info)))
    ;; Skip the "clef" system as that's the name of this LSP
    (when (string-equal system-name "clef")
      (slog :debug "Skipping system 'clef' (self)")
      (return-from load-system-with-info nil))
    (handler-case
        (progn
          ;; Register the .asd directory with ASDF
          (pushnew (directory-namestring asd-path)
                   asdf:*central-registry*
                   :test #'equal)
          ;; Load the .asd file
          (asdf:load-asd asd-path)
          ;; Load the system if it exists
          (when (asdf:find-system system-name nil)
            (slog :debug "Loading system: ~A" system-name)
            (safe-load-system system-name)
            ;; After loading, populate source files from ASDF
            (setf (clef-symbols:system-info-source-files sys-info)
                  (get-system-source-files system-name))
            (setf (clef-symbols:system-info-loaded-p sys-info) t)
            (slog :info "Successfully loaded system: ~A" system-name)))
      (error (e)
        (slog :warn "Failed to load system ~A: ~A" system-name e)))))

(defun build-file-to-system-mapping ()
  "Populate the file-to-system table on *SERVER* from loaded system info."
  (let ((mapping ctx:file-to-system))
    (clrhash mapping)
    (maphash (lambda (system-name sys-info)
               (dolist (file-path (clef-symbols:system-info-source-files sys-info))
                 (setf (gethash file-path mapping) system-name)))
             ctx:loaded-systems)))

(defun load-all-workspace-systems (root-uri)
  "Discover and load all ASDF systems in the workspace."
  (let ((asd-files (discover-asd-files root-uri)))
    (if (null asd-files)
        (slog :warn "No .asd files found in workspace: ~A" root-uri)
        (progn
          (setf ctx:asd-files asd-files)
          (slog :info "Discovered ~A .asd file(s) in workspace" (length asd-files))

          ;; Clear previous state
          (clrhash ctx:loaded-systems)
          (clrhash ctx:file-to-system)

          ;; Phase 1: Parse all .asd files to discover systems
          (dolist (asd-path asd-files)
            (slog :debug "Parsing .asd file: ~A" asd-path)
            (let ((systems (parse-asd-file asd-path)))
              (dolist (sys systems)
                (slog :debug "Found system: ~A" (clef-symbols:system-info-name sys))
                (setf (gethash (clef-symbols:system-info-name sys) ctx:loaded-systems) sys))))

          ;; Phase 2: Determine load order based on dependencies
          (let ((load-order (compute-system-load-order)))
            (slog :debug "System load order: ~A" load-order)

            ;; Phase 3: Load each system in dependency order
            (dolist (system-name load-order)
              (let ((sys-info (gethash system-name ctx:loaded-systems)))
                (when sys-info
                  (load-system-with-info sys-info)))))

          ;; Phase 4: Build file-to-system mapping
          (build-file-to-system-mapping)
          (slog :info "Loaded ~A system(s), mapped ~A file(s)"
                (hash-table-count ctx:loaded-systems)
                (hash-table-count ctx:file-to-system))))))

;;; Utility functions for querying system state

(defun get-file-system (file-path)
  "Get the system name that a file belongs to, or nil if unknown."
  (gethash (namestring file-path) ctx:file-to-system))

(defun list-workspace-systems ()
  "Return a list of all discovered system names."
  (loop for name being the hash-keys of ctx:loaded-systems
        collect name))

(defun handle-initialize (request)
       (let* ((params-hash (clef-jsonrpc/types:request-params request))
              (capabilities (href params-hash "capabilities")))

             ;; Get the workspace root and load the .ASD to power LSP diagnostics & symbols
             (handler-case
               ;; We currently assume one does exist and it's the first value
               (let ((workspace-root (href (aref (href params-hash "workspace-folders") 0) "uri")))
                    (slog :info "Client workspace root: ~A" workspace-root)
                    (setf ctx:workspace-root workspace-root)
                    ;; Load all .asd files (root + test directories)
                    (load-all-workspace-systems workspace-root)
                    (let ((start-time (get-internal-real-time)))
                         (slog :debug "Building project symbol map...")
                         (clef-symbols:build-project-symbol-map (clef-util:cleanup-path workspace-root))
                         (slog :debug "Built project symbol map in ~A ms."
                               (/ (* (- (get-internal-real-time) start-time) 1000.0)
                                  internal-time-units-per-second))))
               (error (e)
                      ;; TODO: Propogate the error and return some specific code?
                      ;; Actually, we can continue initializing the server, but would need to disable some
                      ;; behavior, and notify the client.
                      (slog :error "Failed to get client workspace root: ~A" e)))

             (setf ctx:client-capabilities capabilities)

             ;; TODO: use *server-capabilities*
             ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeResult
             clef-lsp/server:*server-capabilities-json*))
; (dict "capabilities"
;       (dict "textDocumentSync" (dict "change" 2)
;             "documentFormattingProvider" t))))
