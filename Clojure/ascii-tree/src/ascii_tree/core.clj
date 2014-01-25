(ns ascii-tree.core)

(defn leaf?
  [node]
  (or (string? node) (and (== 2 (count node))
                          (integer? (second node)))))

(defn height
  "Returns the height of a given tree."
  [node]
  (if (leaf? node)
    1
    (let [[root & branches] node]
      (inc (reduce max (map height branches))))))

(defn space-str
  "Returns a string of n spaces."
  [n]
  (apply str (repeat n \space)))

(defn make-empty-node
  [w]
  (let [elem (space-str (dec w))]
    (list elem w)))

(defn calc-width
  "Calculates the width of a tree by calculating the width of the leaves first."
  [node]
  (if (leaf? node)
    (list node (count node))
    (let [[root & branches] node
          mod-branches (map calc-width branches)
          branches-len (reduce #(+ %1 (second %2)) 0 mod-branches)
          root-len (count root)]
      ;; If root's width is greater than branches', a fake node is inserted to print
      ;; correctly the leaves in the future.
      (if (< root-len branches-len)
        (list* root branches-len mod-branches)
        (list* root 
               root-len
               (make-empty-node (- root-len branches-len)) 
               mod-branches)))))

(defn print-root 
  [root extra-space]
  (let [[elem w & branches] root 
        n-space (-> (+ w (* extra-space (count branches)))
                    (- (count elem))
                    (/ 2))]
    (if (zero? n-space)
      (print elem "")
      (print (str (space-str (dec n-space)) elem (space-str (inc n-space)))))))

(defn print-leaf
  [leaf extra-space]
  (let [[elem w] leaf]
    (print (str elem (space-str (- (+ extra-space w) (count elem)))))))

(defn print-tree
  "Prints a list/sequence/collection as a tree. The first element is always a root,
  the branches or leaves are the rest of the elements."
  ([tree] (print-tree tree 2))
  ([tree space]
   (let [max-level (dec (height tree))]
     (loop [queue [(calc-width tree)], level 0]
       (when (seq queue)
         (print "\n") ; to split levels.
         (recur 
           (reduce (fn [next-queue node] 
                     ;; Checks if the element has children or not.
                     (if (leaf? node)
                       (let [[leaf w] node]
                         (print-leaf node space)
                         (if (== level max-level)
                           next-queue
                           (conj next-queue (make-empty-node w))))
                       (let [[root w & branches] node]
                         (print-root node space)
                         (reduce #(conj %1 %2) ; enqueue its children for the next level.
                                 next-queue
                                 branches))))
                   [] ; next-level queue
                   queue)
           (inc level)))))
   (print "\n") ; just to separate the tree from the returned value.
   "Finished"))

;; remove "a" before
; (p0 ((b c) (d e) f) nil)
