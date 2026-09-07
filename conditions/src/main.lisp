;;;; The entry module, and the public face of the library.
;;;;
;;;; Under the package convention every file's package is named by its path,
;;;; which leaves a library with no file whose package can simply be called
;;;; `clef-conditions'. This module is the answer: it re-exports the two
;;;; implementation packages and carries the primary system's name as a
;;;; nickname, so consumers write CLEF-CONDITIONS:EXTRACT as they always have.
;;;;
;;;; The nickname is also what inference needs. A consumer declaring
;;;; (:import-from :clef-conditions) maps that name to the system
;;;; `clef-conditions' -- this .asd -- whose only dependency is this file.

(uiop:define-package :clef-conditions/src/main
  (:nicknames :clef-conditions)
  (:use-reexport :clef-conditions/src/extract
                 :clef-conditions/src/render))

(in-package :clef-conditions/src/main)
