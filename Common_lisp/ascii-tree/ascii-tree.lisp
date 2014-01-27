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

(defun fill-str (elem width c)
  "Takes a element (string) and returns a new string with a new width and filled with
  characters c. The first and the last to-fill characters of the new strings are just
  empty spaces.
  Ex.: (fill-str '123' 8 '_')
  => ' __123_ '"
  (let* ((n-tofill (- width (length elem)))
         (half-cfill (/ (- n-tofill 2) 2)))
    (labels ((make-c-str (k)
               ;; Returns a string of Cs whose length depends on the half-cfill and
               ;; if the output is for the left side or for the right.
               (make-string (if (> half-cfill 0) 
                              (if (equalp :l k) (ceiling half-cfill) (floor half-cfill))
                              0) 
                            :initial-element c)))
      (concatenate 'string 
                   (if (> n-tofill 0) " " "")
                   (make-c-str :l)
                   elem 
                   (make-c-str :r)
                   (if (> n-tofill 1) " " "")))))

(defun print-elem! (elem w c)
  (print (fill-str elem w c)))

(defun print-lines! (queue spc)
  (map-queue (lambda (node)
               (let ((w (width node spc)))
                 (if (leaf? node)
                   (print (fill-str " " w #\space)) ; prints just spaces.
                   (let ((middle-root (/ w 2)))
                     (reduce (lambda (acc branch-node)
                               (let ((bw (width branch-node spc)))
                                 (if (< (/ (+ acc bw) 2) middle-root)
                                   (print-elem! "/" bw #\space)
                                   (print-elem! "\\" bw #\space))
                                 (+ acc bw)))
                             (rest node) ; branches
                             :initial-value 0)))))
             queue))

;(defun print-tree! (tree &optional (spc 2))
;  (let ((max-level (1- (height tree))))
;    
;    )
;  )
