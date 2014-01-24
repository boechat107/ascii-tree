(ns ascii-tree.core)

(defn leaf?
  [node]
  (string? node))

(defn length
  [elem]
  (count elem))

(defn calc-length
  "Calculates the length of a tree by calculating the length of the leaves first."
  [node]
  (if (leaf? node)
    (list node (length node))
    (let [[root & branches] node
          mod-branches (map calc-length branches)
          branches-len (reduce #(+ %1 (second %2)) 0 mod-branches)
          root-len (length root)]
      (list* root (max root-len branches-len) mod-branches))))

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
