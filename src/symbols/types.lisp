(in-package :clef-symbols)

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
         `(member :let :flet :labels :lambda :defun :defmacro))

(defstruct location
           "A range (character offset) location within a file of source code."
           (file-path nil :type string)
           ;; The index of the start character of the range
           (start nil :type integer)
           ;; The index of the end character of the range
           (end nil :type integer))

(defstruct symbol-definition
           "A definition of a symbol in the workspace."
           (symbol-name nil :type string)
           (package-name nil :type string)
           (kind nil :type symbol-kind)
           (location nil :type location)
           (defining-scope nil :type lexical-scope))

(defstruct symbol-reference
           "A reference (usage) of a symbol in the workspace."
           (symbol-name nil :type string)
           ;; TODO: Is this necessary? I think it'd be the package that's current at time of use
           ;; (package-name nil :type string)
           (location nil :type location)
           (usage-scope nil :type lexical-scope)
           ;; nil if not resolvable
           (resolved-definition nil :type (or null symbol-definition)))

(defstruct lexical-scope
           (kind nil :type scope-kind)
           (location nil :type location)
           (parent-scope nil :type (or null lexical-scope))
           (defined-symbols nil :type hash-table)
           ;; Should be a list of lexical-scope's
           (child-scopes nil :type list))
