(in-package :clef-context)

;;; Central server context.
;;;
;;; Historically CLEF scattered its mutable state across many defparameters in
;;; several packages (clef-lsp/server, clef-symbols, clef-lsp/lifecycle). That
;;; made it hard to reset between test runs, impossible to host more than one
;;; server in a single image, and awkward when new handlers needed to reach
;;; across module boundaries to touch state.
;;;
;;; This file consolidates everything onto a single SERVER-CONTEXT struct held
;;; in the special variable *SERVER*. Each field corresponds to one of the old
;;; globals. Short symbol-macro aliases expand to field access on *SERVER*,
;;; so call sites look like ordinary variable references:
;;;
;;;     (gethash uri (documents))          ; reads (server-context-documents *server*)
;;;     (setf (workspace-root) path)       ; writes (server-context-workspace-root *server*)
;;;
;;; Tests and shutdown handlers can swap *SERVER* with a fresh context to reset
;;; all state atomically.

(defstruct server-context
  "All persistent state for a running CLEF LSP server.

Fields are grouped by subsystem:

Lifecycle:
  initialized          -- set once the client sends the `initialized' notification
  shutdown-received    -- set when the client sends `shutdown'; used by `exit'
  client-capabilities  -- hash table of capabilities from `initialize'
  workspace-root       -- URI of the project root
  output-stream        -- stream for outbound LSP messages (notifications)

Handlers & documents:
  handlers             -- method-name -> handler-function
  documents            -- document URI -> full text string

Symbol analysis (formerly in clef-symbols):
  lexical-scopes       -- file path -> interval tree of lexical-scope's
  symbol-refs          -- file path -> interval tree of symbol-reference's
  workspace-symbol-index -- symbol-name -> list of symbol-definitions (cross-file)
  document-line-offsets  -- file path -> vector of per-line byte offsets
  global-scope         -- root lexical-scope holding builtins + externals

ASDF systems (formerly in clef-lsp/lifecycle):
  loaded-systems       -- system name -> system-info
  file-to-system       -- absolute file path -> owning system name
  asd-files            -- list of all discovered .asd pathnames"
  (initialized nil :type boolean)
  (shutdown-received nil :type boolean)
  (client-capabilities nil)
  (workspace-root nil)
  (output-stream nil)
  (handlers (make-hash-table :test 'equal))
  (documents (make-hash-table :test 'equal))
  (lexical-scopes (make-hash-table :test 'equal))
  (symbol-refs (make-hash-table :test 'equal))
  (workspace-symbol-index (make-hash-table :test 'equal))
  (document-line-offsets (make-hash-table :test 'equal))
  (global-scope nil)
  (loaded-systems (make-hash-table :test 'equal))
  (file-to-system (make-hash-table :test 'equal))
  (asd-files nil))

(defparameter *server* (make-server-context)
  "The current CLEF LSP server context.

Bound to a freshly constructed SERVER-CONTEXT at image load, replaced on
RESET-CONTEXT, and rebindable with LET for isolated tests.")

(defun reset-context ()
  "Replace *SERVER* with a fresh context, discarding all server state.
Called from shutdown and exit handlers and from test setup."
  (setf *server* (make-server-context)))

;;; Symbol-macro aliases.
;;;
;;; These let callers write `(documents)' or `(setf (workspace-root) x)' as
;;; if they were plain functions/places, without the noise of threading
;;; *server* through every call. Setf works transparently because each
;;; expansion bottoms out in a struct accessor, which has a real setf
;;; expander.

(defmacro define-context-accessor (short-name field-accessor docstring)
  "Define SHORT-NAME as a symbol-macro that reads/writes (FIELD-ACCESSOR *SERVER*)."
  (declare (ignore docstring))
  `(define-symbol-macro ,short-name (,field-accessor *server*)))

(define-context-accessor initialized server-context-initialized
  "Whether the client has confirmed initialization.")

(define-context-accessor shutdown-received server-context-shutdown-received
  "Whether shutdown has been requested.")

(define-context-accessor client-capabilities server-context-client-capabilities
  "Client capabilities reported at initialize time.")

(define-context-accessor workspace-root server-context-workspace-root
  "Project workspace root URI.")

(define-context-accessor output-stream server-context-output-stream
  "Outbound LSP message stream.")

(define-context-accessor handlers server-context-handlers
  "Method name -> handler function.")

(define-context-accessor documents server-context-documents
  "Document URI -> current text.")

(define-context-accessor lexical-scopes server-context-lexical-scopes
  "File path -> interval tree of lexical scopes.")

(define-context-accessor symbol-refs server-context-symbol-refs
  "File path -> interval tree of symbol references.")

(define-context-accessor workspace-symbol-index server-context-workspace-symbol-index
  "Symbol name -> list of symbol-definitions, across all files.")

(define-context-accessor document-line-offsets server-context-document-line-offsets
  "File path -> vector of byte offsets for each line.")

(define-context-accessor global-scope server-context-global-scope
  "Root lexical-scope for the workspace (holds builtins + externals).")

(define-context-accessor loaded-systems server-context-loaded-systems
  "System name -> system-info for ASDF systems discovered in the workspace.")

(define-context-accessor file-to-system server-context-file-to-system
  "Absolute file path -> system name that owns it.")

(define-context-accessor asd-files server-context-asd-files
  "List of every .asd file discovered in the workspace.")
