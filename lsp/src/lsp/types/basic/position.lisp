(in-package :clef-lsp/types/basic)

(defclass position ()
        ((line
          :initarg :line
          :accessor position-line
          :type uinteger
          :documentation "Line position in a document (zero-based).")
         (character
             :initarg :character
             :accessor position-character
             :type uinteger
             :documentation "Character offset on a line in a document (zero-based).")))
