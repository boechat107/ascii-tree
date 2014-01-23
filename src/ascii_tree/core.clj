(ns ascii-tree.core)

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
