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
  "Returns the width of a tree, considering a extra space between leaves."
  (labels ((walker (node)
             (if (leaf? node)
               (+ extra-space (length node))
               (max (reduce #'+ (map 'list #'walker (rest node)))
                    (+ extra-space (length (first node)))))))
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

(defun uniform-width (tree spc)
  "Returns a new tree (a lisp list) where some leaves can be possible expanded by
   spaces to fix their subtrees.
   The algorithm checks if there is some width inconsistency (root wider than the sum
   of its branches) and fix the problem the first possible leaf of the first possible 
   branch."
  (labels ((root-branches-diff (node)
             ;; Calculates the difference between the sum of branches' width and the
             ;; root's width.
             (let* ((root-len (+ spc (length (first node))))
                    (bs-len (reduce (lambda (acc b)
                                      (+ acc (width b spc)))
                                    (rest node)
                                    :initial-value 0)))
               (- root-len bs-len)))
           (walker (node diff)
             (cond 
               ((and (leaf? node) (plusp diff))
                (fill-str node (+ diff (length node)) #\space))
               ((leaf? node) node)
               ;; If the root's width is greater than its branches', its first
               ;; branch's string is expanded with spaces to have a uniform 
               ;; subtree spacing.
               (t (let* ((this-diff (root-branches-diff node))
                         (new-diff (if (> diff this-diff) diff (+ diff this-diff)))
                         (branches (rest node)))
                    (cons 
                      (first node)
                      (cons (walker (first branches) new-diff) 
                            (map 'list (lambda (b) (walker b 0)) (rest branches)))))))))
    (walker tree 0)))

(defun print-elem! (elem w c)
  (format t "~a" (fill-str elem w c)))

(defun print-lines! (queue spc)
  (map-queue (lambda (node)
               (let ((w (width node spc)))
                 (if (leaf? node)
                   (print-elem! " " w #\space) ; prints just spaces.
                   (let ((middle-root (/ w 2)))
                     ;; Uses the middle point of the space used by the root to choose
                     ;; between / our \.
                     ;; The link is printed on the same space of a branch.
                     (reduce (lambda (acc branch-node)
                               (let ((bw (width branch-node spc)))
                                 (if (< (/ (+ acc bw) 2) middle-root)
                                   (print-elem! "/" bw #\space)
                                   (print-elem! "\\" bw #\space))
                                 (+ acc bw)))
                             (rest node) ; branches
                             :initial-value 0)))))
             queue))

(defun print-tree! (tree &optional (spc 2))
  (let ((max-level (1- (height tree)))
        (queue (make-queue :simple-queue)))
    (qpush queue (uniform-width tree spc))
    (labels 
      ((level-loop (cur-q level)
         (when (plusp (qsize cur-q))
           (let ((next-q (make-queue :simple-queue)))
             (map-queue (lambda (node)
                          (let ((w (width node spc)))
                            (if (leaf? node)
                              (progn 
                                (print-elem! node w #\space)
                                ;; When the leaf is not at the last level of the
                                ;; tree, a fake node is inserted as its branch just 
                                ;; fill the empty space below.
                                (when (not (= level max-level))
                                  (qpush next-q 
                                         (fill-str " " (length node) #\space))))
                              (progn 
                                (print-elem! (first node) w #\_)
                                ;; Enqueue its branches for the next level.
                                (map nil (lambda (branch) (qpush next-q branch))
                                     (rest node))))))
                        cur-q)
             (format t "~%") ; to split levels
             (print-lines! cur-q spc)
             (format t "~%") ; to split levels 
             (level-loop next-q (1+ level))))))
      (level-loop queue 0))))
