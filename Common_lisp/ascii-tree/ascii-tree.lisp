;;;; ascii-tree.lisp

(in-package #:ascii-tree)

;;; "ascii-tree" goes here. Hacks and glory await!

(defun leaf? (node)
  (stringp node))

(defun height (tree)
  (if (leaf? tree)
    1
    (1+ (reduce #'max (map 'list #'height (rest tree))))))

(defun width (tree extra-space)
  (labels ((walker (node)
             (if (leaf? node)
               (+ extra-space (length node))
               (reduce #'+ (map 'list #'walker (cdr node))))))
    (walker tree)))
