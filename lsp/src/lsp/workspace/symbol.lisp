(in-package :clef-lsp/workspace)

;; The SymbolKind constants and LISP-KIND-TO-LSP-KIND used to be defined here.
;; They now live in :clef-lsp/types/basic alongside NODE-TO-RANGE, since
;; textDocument/documentSymbol needs the same mapping and a second copy of a
;; lookup table is how two copies drift.

(defparameter *workspace-symbol-limit* 200
  "How many matches to return at most.

There is no way to tell a client that a result set was truncated, so the only
defence is ranking: if the cut has to happen, it happens after sorting and takes
the least relevant matches rather than an arbitrary hundred.")

(defun match-score (query name)
  "How well NAME matches QUERY. Lower is better; NIL means no match.

Ranked at all, which it was not before. An exact match should not be buried
under thirty substring hits, and on a workspace of any size it was -- the old
code took whatever hundred the hash table happened to yield first."
  (let ((q (string-upcase query))
        (n (string-upcase name)))
    (cond ((string= q "") 3)
          ((string= q n) 0)
          ((and (<= (length q) (length n))
                (string= q n :end2 (length q)))
           1)
          ((search q n) 2)
          (t nil))))

(defun symbol-info-sort-key (entry)
  "(score name-length name) for one (score . info) pair."
  (destructuring-bind (score . info) entry
    (let ((name (gethash "name" info)))
      (list score (length name) name))))

(defun sort-key< (a b)
  (destructuring-bind (score-a length-a name-a) a
    (destructuring-bind (score-b length-b name-b) b
      (cond ((/= score-a score-b) (< score-a score-b))
            ;; A shorter name containing the query is a closer match than a
            ;; longer one: searching "token" should surface TOKEN before
            ;; SORT-AND-DEDUPE-TOKENS.
            ((/= length-a length-b) (< length-a length-b))
            (t (string< name-a name-b))))))

(defun handle-workspace-symbol (message)
  "Handle a workspace/symbol request.
Returns symbols matching the query from across the workspace, best first."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (query (or (href params "query") ""))
         (matches '()))
    (slog :debug "[workspace/symbol] Query: ~A" query)
    (maphash (lambda (symbol-name defs)
               (let ((score (match-score query symbol-name)))
                 (when score
                   (dolist (def defs)
                     (push (cons score (symbol-def-to-symbol-info def symbol-name))
                           matches)))))
             ctx:workspace-symbol-index)
    (let* ((sorted (sort matches #'sort-key< :key #'symbol-info-sort-key))
           (total (length sorted))
           (kept (if (> total *workspace-symbol-limit*)
                     (subseq sorted 0 *workspace-symbol-limit*)
                     sorted)))
      (when (> total *workspace-symbol-limit*)
        ;; The client cannot be told, so at least the log can.
        (slog :info "[workspace/symbol] ~A matches for ~S, returning the best ~A"
              total query *workspace-symbol-limit*))
      (slog :debug "[workspace/symbol] Found ~A matching symbols" total)
      (coerce (mapcar #'cdr kept) 'vector))))

(defun symbol-def-to-symbol-info (def symbol-name)
  "Convert a symbol-definition to an LSP SymbolInformation dict."
  (let* ((location (clef-symbols:symbol-definition-location def))
         (file-path (clef-symbols:location-file-path location))
         (node (clef-symbols:symbol-definition-node def))
         (kind (clef-symbols:symbol-definition-kind def))
         (package (clef-symbols:symbol-definition-package-name def)))
    (dict "name" symbol-name
          "kind" (lisp-kind-to-lsp-kind kind)
          ;; The package, which is what tells two same-named results apart.
          ;; Without it a search for a common name returns several identical
          ;; looking rows and the only way to choose is to open each one.
          ;; CONTAINERNAME is the spec's field for exactly this.
          "containerName" (if package
                              (string-upcase (princ-to-string package))
                              "")
          "location" (dict "uri" (format nil "file://~A" file-path)
                           "range" (node-to-range node)))))

;; NODE-TO-RANGE and LISP-KIND-TO-LSP-KIND are imported from
;; :clef-lsp/types/basic, which is where the one copy of each lives.
