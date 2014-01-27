(ns ascii-tree.core)

(defn leaf?
  "Returns true if the given node is a leaf. It should work for the modified tree
  returned by add-width-info."
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

(defn fill-str
  "Takes a element (string) and returns a new string with a new width and filled with
  characters c. The first and the last to-fill characters of the new strings are just
  empty spaces.
  Ex.: (fill-str '123' 8 '_')
  => '  _123_ '"
  [elem width c]
  (let [to-fill (/ (- width (count elem)) 2)
        fill-c (if (< to-fill 2) "" (repeat-char (dec to-fill) c))
        fill-spc (if (< to-fill 1) "" \space)
        output (format "%s%s%s%s%s" fill-spc fill-c elem fill-c fill-spc)]
    (if (< (count output) width)
      (str \space output)
      output)))

(defn add-width-info
  "Calculates the width of a tree by calculating the width of its subtrees first.
  The width information is stored as the second element of a node. Even leaves are
  represented by lists now."
  [tree extra-space]
  (letfn [(walker [node]
            (if (leaf? node)
              (list node (+ extra-space (count node)))
              (let [[root & branches] node
                    mod-branches (map walker branches)
                    branches-width (reduce #(+ %1 (second %2)) 0 mod-branches)
                    root-width (+ extra-space (count root))]
                ;; If root's width is greater than branches', the first branch is
                ;; expanded with \space characters until the minimum subtree's width.
                (if (< root-width branches-width)
                  (list* root branches-width mod-branches)
                  (let [[e w & bs] (first mod-branches)
                        new-width (+ w (- root-width branches-width))
                        mod-node (list* (fill-str e new-width \space) new-width bs)]
                    (list* root 
                           root-width
                           mod-node
                           (rest mod-branches)))))))]
    (walker tree)))

;;; Functions to print on the screen.

(defn print-elem!
  "Prints the content of nodes, filling the output string with characters fill-ch if
  element's width is less than w."
  [elem w fill-ch]
  (print (fill-str elem w fill-ch)))

(defn print-lines!
  "Given the queue of nodes of a specific level, prints the lines to connect roots
  and branches."
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

(defn print-tree!
  "Prints a list/sequence/collection as a tree. The first element is always a root,
  the branches or leaves are the rest of the elements.
  The space between the elements can be specified."
  ([tree] (print-tree! tree 2))
  ([tree space]
   (let [max-level (dec (height tree))]
     (loop [queue [(add-width-info tree space)], level 0]
       (when (seq queue)
         (let [next-queue (reduce (fn [next-queue node] 
                                    ;; Checks if the element has children or not.
                                    (if (leaf? node)
                                      (let [[leaf w] node]
                                        (print-elem! leaf w \space)
                                        ;; If a leaf is not at the last level, a fake
                                        ;; node is inserted to fill all the empty
                                        ;; space below.
                                        (if (== level max-level)
                                          next-queue
                                          (conj next-queue 
                                                (list (repeat-char w \space) w))))
                                      (let [[root w & branches] node]
                                        (print-elem! root w "_")
                                        ;; Enqueue its children for the next level.
                                        (reduce #(conj %1 %2) 
                                                next-queue
                                                branches))))
                                  [] ; next-level queue
                                  queue)]
         (newline) ; to split levels.
         (print-lines! queue space)
         (newline) ; to split levels.
         (recur next-queue (inc level))))))
    (newline) ; just to separate the tree from the returned value.
   "Finished"))
