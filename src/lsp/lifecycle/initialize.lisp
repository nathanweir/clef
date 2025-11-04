(in-package :clef-lsp/lifecycle)



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
              (wildcard-path (concatenate 'string path-root "*.asd"))
              (asd-files (uiop:directory* wildcard-path)))
             (if asd-files
                 (load-asd (first asd-files))
                 ;; TODO: Throw an error? Need to decide on how to handle cases where we can't
                 ;; load .asd
                 (slog :debug "No .asd files found in workspace root: ~A" wildcard-path))))

(defun handle-initialize (request)
       (let* ((params-hash (clef-jsonrpc/types:request-params request))
              (capabilities (href params-hash "capabilities")))

             ;; Get the workspace root and load the .ASD to power LSP diagnostics & symbols
             (handler-case
               ;; We currently assume one does exist and it's the first value
               (let ((workspace-root (href (aref (href params-hash "workspace-folders") 0) "uri")))
                    (slog :info "Client workspace root: ~A" workspace-root)
                    (load-workspace-asd workspace-root))
               (error (e)
                      ;; TODO: Propogate the error and return some specific code?
                      ;; Actually, we can continue initializing the server, but would need to disable some
                      ;; behavior, and notify the client.
                      (slog :error "Failed to get client workspace root: ~A" e)))


             (setf clef-lsp/server:*client-capabilities* capabilities)

             ;; TODO: use *server-capabilities*
             ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initializeResult
             clef-lsp/server:*server-capabilities-json*))
; (dict "capabilities"
;       (dict "textDocumentSync" (dict "change" 2)
;             "documentFormattingProvider" t))))
