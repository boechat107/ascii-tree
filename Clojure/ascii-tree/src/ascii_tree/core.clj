(ns ascii-tree.core)

(defn leaf?
  [node]
  (or (string? node) (and (== 2 (count node))
                          (integer? (second node)))))

(defn width
  [elem]
  (inc (count elem)))

(defn height
  "Returns the height of a given tree."
  [node]
  (if (leaf? node)
    1
    (let [[root & branches] node]
      (inc (reduce max (map height branches))))))

(defn calc-width
  "Calculates the width of a tree by calculating the width of the leaves first."
  [node]
  (if (leaf? node)
    (list node (width node))
    (let [[root & branches] node
          mod-branches (map calc-width branches)
          branches-len (reduce #(+ %1 (second %2)) 0 mod-branches)
          root-len (width root)]
      (list* root (max root-len branches-len) mod-branches))))

(defn space-str
  "Returns a string of n spaces."
  [n]
  (apply str (repeat n \space)))

(defn make-empty-node
  [w]
  (let [elem (space-str (dec w))]
    (list elem w)))

(defn print-root 
  [root-elem w]
  (let [n-space (/ (- w (width root-elem)) 2)]
    (if (zero? n-space)
      (print root-elem "")
      (print (str (space-str (dec n-space)) root-elem (space-str (inc n-space)))))))

(defn print-tree
  "Prints a list/sequence/collection as a tree. The first element is always a root,
  the branches or leaves are the rest of the elements."
  [tree]
  (let [max-level (dec (height tree))]
    (loop [queue [(calc-width tree)], level 0]
      (when (seq queue)
        (print "\n") ; to split levels.
        (recur 
          (reduce (fn [next-queue node] 
                    ;; Checks if the element has children or not.
                    (if (leaf? node)
                      (let [[leaf w] node]
                        (print leaf "")
                        (if (== level max-level)
                          next-queue
                          (conj next-queue (make-empty-node w))))
                      (let [[root w & branches] node]
                        (print-root root w)
                        (reduce #(conj %1 %2) ; enqueue its children for the next level.
                                next-queue
                                branches))))
                  [] ; next-level queue
                  queue)
          (inc level)))))
  (print "\n") ; just to separate the tree from the returned value.
  "Finished")

;; remove "a" before
; (p0 ((b c) (d e) f) nil)
