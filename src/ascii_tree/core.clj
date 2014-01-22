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
  (loop [queue [tree]]
    (when (seq queue)
      (print "\n")
      (recur 
        (reduce (fn [next-queue sub-tree] 
                  (if (coll? sub-tree)
                    (let [[root & children] sub-tree]
                      (print root " ")
                      (reduce #(conj %1 %2) 
                              next-queue
                              children))
                    (do (print sub-tree " ")
                        next-queue)))
                [] ; next-level queue
                queue))))
  (print "\n")
  "Finished")

;; remove "a" before
; (p0 ((b c) (d e) f) nil)
