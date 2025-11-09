(in-package :clef-symbols)

;; Custom struct for adding data to the :cl-interval interval tree
(defstruct (clef-interval (:include interval:interval))
           data)

;; ;; Define methods so cl-interval can access the interval data
;; (defmethod interval:interval-start ((obj interval-data))
;;            (interval:interval-start (interval-data-interval obj)))

;; (defmethod interval:interval-end ((obj interval-data))
;;            (interval:interval-end (interval-data-interval obj)))


;; TODO: No idea how much this will be used or what's appropriate
(defparameter +symbol-kinds+ '(:variable :function :macro :class :package :constant :type)
              "Enumeration of possible kinds of symbols.")

(deftype symbol-kind ()
         "An enum type for symbol kinds."
         `(member :variable :function :macro :class :package :constant :type))

(defparameter +scope-kinds+ '(:let :flet :labels :lambda :defun :defmacro)
              "Enumeration of possible kinds of scope bindings.")

(deftype scope-kind ()
         "An enum type for scope kinds."
         `(member :document :let :flet :labels :lambda :defun :defmacro))

(defstruct location
           "A range (character offset) location within a file of source code."
           (file-path nil :type string)
           ;; The index of the start character of the range
           (start nil :type integer)
           ;; The index of the end character of the range
           (end nil :type integer))
;; TODO: Bring these back in if it ends up being more convenient to track the original
;; start & end line/char than it is to recalculate at time of use
;; These line & character vals are translated into start/end above upon creation
;; Original line number for position in the source doc
;; (start-line nil :type integer)
;; ;; Original character index for position in the source doc
;; (start-character nil :type integer)
;; (end-line nil :type integer)
;; (end-character nil :type integer))

(defstruct symbol-definition
           "A definition of a symbol in the workspace."
           (symbol-name nil :type string)
           (package-name nil :type symbol)
           (kind nil :type symbol-kind)
           (location nil :type location)
           (defining-scope nil :type lexical-scope))

(defstruct symbol-reference
           "A reference (usage) of a symbol in the workspace."
           (symbol-name nil :type string)
           ;; TODO: Is this necessary? I think it'd be the package that's current at time of use
           ;; (package-name nil :type string)
           (location nil :type location)
           (usage-scope nil :type lexical-scope))
;; TODO: Could pre-compute this, for but now probably easier to just calculate by walking up the tree
;; at time of need.
;; nil if not resolvable
;; (resolved-definition nil :type (or null symbol-definition)))

(defstruct lexical-scope
           (kind nil :type scope-kind)
           (location nil :type location)
           ;; If null then this scope is the document root
           (parent-scope nil :type (or null lexical-scope))
           ;; Should be a list of symbol-definition's
           (symbol-definitions nil :type list)
           (symbol-references nil :type hash-table)
           ;; Should be a list of lexical-scope's
           (child-scopes nil :type list))
