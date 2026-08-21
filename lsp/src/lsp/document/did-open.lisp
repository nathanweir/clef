(in-package :clef-lsp/document)

;;; Document lifecycle: didOpen and didClose.
;;;
;;; didChange lives next door and shares INDEX-DOCUMENT with this file.

(defun index-document (document-uri text)
       "Store TEXT for DOCUMENT-URI and rebuild its symbol map.

Storing without indexing is what didOpen used to do, and it made navigation
depend on having typed into a file. A document that the workspace scan had not
already covered -- one just created, one outside any .asd, one in a workspace
whose scan failed -- had no definitions, no references and no highlights until
the first edit produced a didChange. Every test in the suite worked around this
by sending a redundant didChange straight after didOpen, which is what kept it
hidden. See docs/surveys/lsp-review.md §1.4."
       (setf (gethash document-uri ctx:documents) text)
       (clef-symbols:build-file-symbol-map (clef-util:cleanup-path document-uri) text))

(defun handle-text-document-did-open (message)
       (let* ((params-hash (clef-jsonrpc/types:request-params message))
              (document-uri (href params-hash "text-document" "uri"))
              (document-text (href params-hash "text-document" "text")))
             (slog :debug "[textDocument/didOpen] Document: ~A" document-uri)
             (index-document document-uri document-text)))

(defun handle-text-document-did-close (message)
       "Drop the client's copy of a document.

The capabilities have always advertised openClose, so clients have always been
entitled to send this; there was simply no handler, and ctx:documents only ever
grew. A long editing session accumulated every file ever opened, and a closed
file kept serving whatever text it had when it was last edited.

The symbol map is deliberately NOT discarded. didClose means the client is no
longer tracking the file, not that the file stopped existing -- its definitions
still belong in the workspace index, and dropping them would make go-to-definition
fail for anything not currently open."
       (let* ((params (clef-jsonrpc/types:request-params message))
              (document-uri (href params "text-document" "uri")))
             (slog :debug "[textDocument/didClose] Document: ~A" document-uri)
             (remhash document-uri ctx:documents)
             nil))
