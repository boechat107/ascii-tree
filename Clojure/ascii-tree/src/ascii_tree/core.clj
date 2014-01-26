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
  (apply str (repeat (int n) \space)))

(defn make-empty-node
  [elem-w extra-space]
  (list (space-str elem-w) (+ elem-w extra-space)))

(defn calc-width
  "Calculates the width of a tree by calculating the width of the leaves first."
  [tree extra-space]
  (letfn [(walker [node]
            (if (leaf? node)
              (list node (+ extra-space (count node)))
              (let [[root & branches] node
                    mod-branches (map walker branches)
                    branches-len (reduce #(+ %1 (second %2)) 0 mod-branches)
                    root-len (count root)]
                ;; If root's width is greater than branches', a fake node is inserted
                ;; to print correctly the leaves in the future.
                (if (< root-len branches-len)
                  (list* root branches-len mod-branches)
                  (list* root 
                         root-len
                         (make-empty-node (- root-len branches-len) extra-space) 
                         mod-branches)))))]
    (walker tree)))

(defn fill-str
  [elem width]
  (let [space (space-str (/ (- width (count elem)) 2))
        output (str space elem space)]
    (if (< (count output) width)
      (str " " output)
      output)))

(defn print-root! 
  [root extra-space]
  (let [[elem w & branches] root 
        ]
    ))

(defn print-leaf!
  [leaf]
  (let [[elem w] leaf]
    (print (fill-str elem w))))

(defn print-lines!
  [queue extra-space]
  (letfn [(printer [brs line-char]
            (doseq [[e w] brs]
              (let [space (space-str (dec (/ w 2)))]
                (print (str space line-char space)))))]
    (doseq [node queue]
      (if (leaf? node)
        (print-leaf! [(space-str (count (first node))) (second node)])
        (let [[root w & branches] node
              middle-root (/ w 2)
              n-branches (count branches)
              [left right] (split-at (/ n-branches 2) branches)
              [left center] (if (odd? n-branches) 
                              [(butlast left) [(last left)]]
                              [left nil])]
          (reduce (fn [acc [_ bw]]
                    (if (< (/ (+ acc bw) 2) middle-root)
                      (print-leaf! ["/" bw])
                      (print-leaf! ["\\" bw]))
                    (+ acc bw))
                  0
                  branches)
          ;(printer left "/")
          ;(when (seq center) (printer center "|"))
          ;(printer right "\\")
          )))))

(defn print-tree
  "Prints a list/sequence/collection as a tree. The first element is always a root,
  the branches or leaves are the rest of the elements."
  ([tree] (print-tree tree 2))
  ([tree space]
   (let [max-level (dec (height tree))]
     (loop [queue [(calc-width tree space)], level 0]
       (when (seq queue)
         (let [next-queue (reduce (fn [next-queue node] 
                                    ;; Checks if the element has children or not.
                                    (if (leaf? node)
                                      (let [[leaf w] node]
                                        (print-leaf! node)
                                        (if (== level max-level)
                                          next-queue
                                          (conj next-queue 
                                                (make-empty-node (count leaf) space))))
                                      (let [[root w & branches] node]
                                        (print-leaf! node)
                                        ;; Enqueue its children for the next level.
                                        (reduce #(conj %1 %2) 
                                                next-queue
                                                branches))))
                                  [] ; next-level queue
                                  queue)]
         (print \newline) ; to split levels.
         (print-lines! queue space)
         (print \newline) ; to split levels.
         (recur next-queue (inc level))))))
   (print "\n") ; just to separate the tree from the returned value.
   "Finished"))

;; remove "a" before
; (p0 ((b c) (d e) f) nil)
