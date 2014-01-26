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

(defn repeat-char
  "Returns a string of n spaces."
  [n c]
  (apply str (repeat (int n) c)))

(defn make-empty-node
  [elem-w extra-space]
  (list (repeat-char elem-w \space) (+ elem-w extra-space)))

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
  [elem width c]
  (let [to-fill (/ (- width (count elem)) 2)
        fill-c (if (< to-fill 2) "" (repeat-char (dec to-fill) c))
        fill-spc (if (< to-fill 1) "" \space)
        output (format "%s%s%s%s%s" fill-spc fill-c elem fill-c fill-spc)]
    (if (< (count output) width)
      (str \space output)
      output)))

(defn print-elem!
  [elem w fill-ch]
  (print (fill-str elem w fill-ch)))

(defn print-lines!
  [queue extra-space]
  (doseq [node queue]
    (if (leaf? node)
      (let [[elem w] node] (print-elem! (repeat-char w \space) w \space))
      (let [[root w & branches] node
            middle-root (/ w 2)]
        (reduce (fn [acc [_ bw]]
                  (if (< (/ (+ acc bw) 2) middle-root)
                    (print-elem! "/" bw \space)
                    (print-elem! "\\" bw \space))
                  (+ acc bw))
                0
                branches)))))

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
                                        (print-elem! leaf w \space)
                                        (if (== level max-level)
                                          next-queue
                                          (conj next-queue 
                                                (make-empty-node (count leaf) space))))
                                      (let [[root w & branches] node]
                                        (print-elem! root w "_")
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
