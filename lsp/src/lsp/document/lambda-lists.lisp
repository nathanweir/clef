(in-package :clef-lsp/document)

;;;; Lambda-list handling, shared by hover, signature-help and inlay-hint.
;;;;
;;;; All three ask SB-INTROSPECT for a lambda list and then walk it, and all
;;;; three walked it with DOLIST, MAPCAR or LOOP FOR ... IN -- every one of
;;;; which assumes a proper list.
;;;;
;;;; Lambda lists are not always proper. A macro lambda list may be DOTTED:
;;;;
;;;;     (define-method-combination name . args)
;;;;
;;;; where `. args' is shorthand for `&rest args'. This is not exotic -- it is
;;;; how DEFINE-METHOD-COMBINATION's own lambda list is written, and
;;;; SB-INTROSPECT reports it faithfully as (NAME . ARGS).
;;;;
;;;; The consequences differed by caller, which is why it took a corpus file to
;;;; find: hover answered "Internal server error" over that one symbol,
;;;; signature-help silently returned nothing (its HANDLER-CASE swallowed it),
;;;; and inlay-hint depended on where the error landed. Found by
;;;; docs/experiments/lsp/corpus/07-defining-forms.lisp, which exists to name
;;;; all eighteen defining forms in one file.

(defun lambda-list-marker-p (item)
       "Is ITEM a lambda-list marker -- &OPTIONAL, &REST, &KEY, &BODY, &AUX?"
       (and (symbolp item)
            item
            (plusp (length (symbol-name item)))
            (char= #\& (char (symbol-name item) 0))))

(defun normalize-lambda-list (lambda-list)
       "LAMBDA-LIST as a proper list, with any dotted tail made explicit.

     (name . args)  ->  (name &rest args)

Semantically identical -- the dot IS &rest -- so every caller can then walk the
result with ordinary list operations. A proper list is returned unchanged.

Returns NIL for a non-list, so callers need not guard separately."
       (cond
         ((null lambda-list) '())
         ;; A bare symbol is not a lambda list; SB-INTROSPECT can return one for
         ;; things it has no arglist for.
         ((not (consp lambda-list)) '())
         (t
           (let ((result '())
                 (rest lambda-list))
                (loop
                  (cond
                    ((null rest) (return))
                    ;; The dotted tail. Everything after the dot is the &REST
                    ;; parameter.
                    ((not (consp rest))
                     (push '&rest result)
                     (push rest result)
                     (return))
                    (t (push (car rest) result)
                       (setf rest (cdr rest)))))
                (nreverse result)))))

(defun trim-lambda-list (lambda-list)
       "The required parameters of LAMBDA-LIST, in order.

Stops at the first marker. Arguments after &OPTIONAL may or may not be present
and arguments after &KEY are already named at the call site, so in neither case
does a positional label say anything true."
       (loop for item in (normalize-lambda-list lambda-list)
             until (lambda-list-marker-p item)
             collect (if (consp item) (first item) item)))
