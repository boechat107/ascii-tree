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

(defn p1
  [tree]
  (loop [queue [tree "\n"]]
    (when (seq queue)
      (let [sub-tree (first queue)]
        (if (coll? sub-tree)
          (let [[root & children] (first queue)]
            (print root " ")
            (recur (reduce #(conj %1 %2) 
                           (vec (rest queue))
                           children)))
          (do (print sub-tree " ")
              (recur (rest queue))))
        ))))

;; remove "a" before
; (p0 ((b c) (d e) f) nil)
