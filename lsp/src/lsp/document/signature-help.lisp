(in-package :clef-lsp/document)

(defun handle-text-document-signature-help (message)
  "Handle a textDocument/signatureHelp request.
Returns signature information for the function call at the cursor position."
  (let* ((params (clef-jsonrpc/types:request-params message))
         (document-uri (href params "text-document" "uri"))
         (position (href params "position"))
         (line (href position "line"))
         (character (href position "character"))
         (document-text (gethash document-uri ctx:documents)))
    (slog :debug "[textDocument/signatureHelp] Document: ~A" document-uri)
    (slog :debug "[textDocument/signatureHelp] Position: line ~A, char ~A" line character)

    (unless document-text
      (slog :warn "[textDocument/signatureHelp] Document not found")
      (return-from handle-text-document-signature-help nil))

    ;; Find the function call context at cursor position
    (multiple-value-bind (func-name arg-index)
        (find-function-call-context document-text line character)
      (slog :debug "[textDocument/signatureHelp] Function: ~A, arg index: ~A" func-name arg-index)

      (unless func-name
        (return-from handle-text-document-signature-help nil))

      ;; Look up the function's arglist
      (let ((arglist (get-function-arglist func-name)))
        (slog :debug "[textDocument/signatureHelp] Arglist: ~A" arglist)

        (unless arglist
          (return-from handle-text-document-signature-help nil))

        ;; Build the signature help response
        (make-signature-help-response func-name arglist arg-index)))))

(defun find-function-call-context (text line character)
  "Find the function being called at the cursor position.
Returns (values function-name argument-index) or (values nil nil).
argument-index is 0-based index of which argument the cursor is in."
  (let* ((lines (split-string-by-newline text))
         (cursor-offset (line-char-to-offset lines line character)))
    (when (null cursor-offset)
      (return-from find-function-call-context (values nil nil)))

    ;; Scan backwards from cursor to find the opening paren and function name
    (let ((paren-depth 0)
          (arg-count 0)
          (in-string nil)
          (func-start nil)
          (func-end nil))

      ;; First, find the opening paren of our function call
      (loop for i from (1- cursor-offset) downto 0
            for ch = (char text i)
            do (cond
                 ;; Track string state (simplified - doesn't handle escapes perfectly)
                 ((char= ch #\")
                  (setf in-string (not in-string)))
                 ;; Skip if in string
                 (in-string nil)
                 ;; Closing paren increases depth (we're going backwards)
                 ((char= ch #\))
                  (incf paren-depth))
                 ;; Opening paren
                 ((char= ch #\()
                  (if (> paren-depth 0)
                      (decf paren-depth)
                      ;; Found our function call's opening paren
                      (progn
                        (setf func-start (1+ i))
                        (return))))))

      (unless func-start
        (return-from find-function-call-context (values nil nil)))

      ;; Find the end of the function name (first space or paren after func-start)
      (setf func-end func-start)
      (loop for i from func-start below (length text)
            for ch = (char text i)
            while (and (not (char= ch #\Space))
                       (not (char= ch #\Newline))
                       (not (char= ch #\Tab))
                       (not (char= ch #\())
                       (not (char= ch #\))))
            do (setf func-end (1+ i)))

      (when (= func-start func-end)
        (return-from find-function-call-context (values nil nil)))

      (let ((func-name (string-downcase (subseq text func-start func-end))))
        ;; Now count arguments from func-end to cursor
        ;; Arguments are separated by whitespace (outside of nested parens/strings)
        (setf paren-depth 0)
        (setf in-string nil)
        (setf arg-count 0)
        (let ((seen-non-space nil))
          (loop for i from func-end below cursor-offset
                for ch = (char text i)
                do (cond
                     ((char= ch #\")
                      (setf in-string (not in-string))
                      (setf seen-non-space t))
                     (in-string
                      (setf seen-non-space t))
                     ((char= ch #\()
                      (incf paren-depth)
                      (setf seen-non-space t))
                     ((char= ch #\))
                      (decf paren-depth)
                      (setf seen-non-space t))
                     ((> paren-depth 0)
                      (setf seen-non-space t))
                     ((member ch '(#\Space #\Newline #\Tab))
                      (when seen-non-space
                        (incf arg-count)
                        (setf seen-non-space nil)))
                     (t
                      (setf seen-non-space t)))))

        (values func-name arg-count)))))

(defun split-string-by-newline (string)
  "Split a string into lines."
  (let ((lines '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) #\Newline)
          do (push (subseq string start i) lines)
             (setf start (1+ i)))
    (push (subseq string start) lines)
    (nreverse lines)))

(defun line-char-to-offset (lines line character)
  "Convert line/character position to absolute offset in text."
  (let ((offset 0))
    (loop for i from 0 below line
          for l in lines
          do (incf offset (1+ (length l)))) ; +1 for newline
    (when (< line (length lines))
      (let ((current-line (nth line lines)))
        (when (<= character (length current-line))
          (+ offset character))))))

(defun parse-package-qualified-name (func-name)
  "Parse a potentially package-qualified name like 'pkg:symbol' or 'pkg::symbol'.
Returns (values symbol-name package-name) where package-name may be nil."
  (let ((double-colon (search "::" func-name))
        (single-colon (position #\: func-name)))
    (cond
      (double-colon
       (values (subseq func-name (+ double-colon 2))
               (subseq func-name 0 double-colon)))
      (single-colon
       (values (subseq func-name (1+ single-colon))
               (subseq func-name 0 single-colon)))
      (t
       (values func-name nil)))))

(defun get-function-arglist (func-name)
  "Get the argument list for a function.
Tries workspace index first, then falls back to sb-introspect for loaded functions."
  (multiple-value-bind (bare-name pkg-name)
      (parse-package-qualified-name func-name)

    ;; First try our workspace index (with bare name, without package prefix)
    (let ((defs (clef-symbols:lookup-in-workspace-index bare-name)))
      (when defs
        (let* ((def (first defs))
               (node (clef-symbols:symbol-definition-node def)))
          (when node
            ;; Try to extract arglist from the definition's scope
            (let ((scope (clef-symbols:symbol-definition-defining-scope def)))
              (when (and scope (eq (clef-symbols:lexical-scope-kind scope) :defun))
                ;; Get parameter names from the scope's symbol definitions
                (let ((params (remove-if-not
                                (lambda (d)
                                  (eq (clef-symbols:symbol-definition-kind d) :parameter))
                                (clef-symbols:lexical-scope-symbol-definitions scope))))
                  (when params
                    (return-from get-function-arglist
                      (mapcar #'clef-symbols:symbol-definition-symbol-name
                              (reverse params)))))))))))

    ;; Fall back to sb-introspect for loaded functions
    (handler-case
        (let* ((pkg (if pkg-name
                        (find-package (string-upcase pkg-name))
                        *package*))
               (sym (when pkg
                      (find-symbol (string-upcase bare-name) pkg))))
          (when (and sym (fboundp sym))
            ;; NORMALIZE: a dotted lambda list would make MAPCAR signal, and
            ;; the HANDLER-CASE below would turn that into a silent "no
            ;; signature" rather than an error anyone would notice.
            (let ((arglist (normalize-lambda-list
                            (sb-introspect:function-lambda-list sym))))
              (when arglist
                (mapcar (lambda (arg)
                          (if (listp arg)
                              (format nil "~(~A~)" arg)
                              (string-downcase (symbol-name arg))))
                        arglist)))))
      (error () nil))))

(defun make-signature-help-response (func-name arglist active-param)
  "Build an LSP SignatureHelp response."
  (let* ((params (mapcar (lambda (arg)
                           (dict "label" (format nil "~A" arg)))
                         arglist))
         (signature-label (format nil "(~A~{ ~A~})" func-name arglist))
         (signature (dict "label" signature-label
                          "parameters" (coerce params 'vector))))
    ;; Only set activeParameter if it's valid
    (when (and active-param (< active-param (length arglist)))
      (setf (gethash "activeParameter" signature) active-param))

    (dict "signatures" (vector signature)
          "activeSignature" 0
          "activeParameter" (or active-param 0))))
