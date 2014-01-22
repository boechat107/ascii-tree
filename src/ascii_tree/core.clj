(ns ascii-tree.core)

(defn p0
  [roots children]
  ;(redl.core/break)
  (if (and (empty? roots) (empty? children))
    (println "Finished")
    (if (seq roots)
      (let [[root & more] roots]
        (if (list? root)
          (do (print (first root) " ")
              (p0 more (cons (rest root) children)))
          (do (print root " ")
              (p0 more children))))
      (do (print "\n")
          (p0 (reverse children) nil))
      )))

(defn print-tree
  "Prints a list/sequence/collection as a tree. The first element is always a root,
  the branches or leaves are the rest of the elements."
  [tree]
  (loop [queue [tree]]
    (when (seq queue)
      (print "\n") ; to split levels.
      (recur 
        (reduce (fn [next-queue sub-tree] 
                  ;; Checks if the element has children or not.
                  (if (coll? sub-tree)
                    (let [[root & children] sub-tree]
                      (print root " ") ; prints the root.
                      (reduce #(conj %1 %2) ; enqueue its children for the next level.
                              next-queue
                              children))
                    (do (print sub-tree " ")
                        next-queue)))
                [] ; next-level queue
                queue))))
  (print "\n") ; just to separate the tree from the returned value.
  "Finished")

;; remove "a" before
; (p0 ((b c) (d e) f) nil)
