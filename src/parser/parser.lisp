(in-package :clef-parser/parser)

;; TODO: Make this path configurable / auto-discovered
;; I'm surprised I can use a path to the .so, here!
(cl-tree-sitter:register-language :commonlisp "/home/nathan/dev/clef/src/parser/tree-sitter-commonlisp")

(declaim (ftype (function (string)) parse-file))
(defun parse-file (file-path)
       (with-open-file (in file-path
                           :direction :input)
                       (let ((contents (let ((str (make-string (file-length in))))
                                            (read-sequence str in)
                                            str)))
                            (cl-tree-sitter:parse-string :commonlisp contents))))

(declaim (ftype (function (string)) ts-parse-string))
(defun parse-string (input)
       (cl-tree-sitter:parse-string :commonlisp input))

;; These helper functions are just to bridge the gap of missing functions from Claude
;; They could have more ergonomic names and be placed along other tree sitter utils

(defun node-start-point-column (node)
       (first (first (cl-tree-sitter:node-range node))))

(defun node-start-point-row (node)
       (second (first (cl-tree-sitter:node-range node))))

(defun node-end-point-column (node)
       (first (second (cl-tree-sitter:node-range node))))

(defun node-end-point-row (node)
       (second (second (cl-tree-sitter:node-range node))))

(defun node-text (node source)
       " Return the text slice for NODE from SOURCE using character offsets."
       (let* ((start-row (node-start-point-row node))
              (start-col (node-start-point-column node))
              (end-row (node-end-point-row node))
              (end-col (node-end-point-column node)))
             ;; (format t "Node from (~A,~A) to (~A,~A)~%" start-row start-col end-row end-col)
             (labels ((line-start-offset (str row)
                                         (let ((pos 0)
                                               (count 0)
                                               (len (length str)))
                                              (if (= row 0)
                                                  0
                                                  (loop while (< pos len)
                                                        do (when (char= (char str pos) #\Newline)
                                                                 (incf count))
                                                        (incf pos)
                                                        when (= count row) do (return pos)))
                                              pos)))
                     (let* ((start-abs (+ (line-start-offset source start-row) start-col))
                            (end-abs (+ (line-start-offset source end-row) end-col))
                            (len (length source)))
                           ;; (format t "Extracting text from ~A to ~A (len ~A)~%" start-abs end-abs len)
                           (subseq source start-abs (min end-abs len))))))
