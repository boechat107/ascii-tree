(ns ascii-tree.core-test
  (:use clojure.test
        ascii-tree.core))

(defn length-test []
  (let [t1 '("a" ("bc" "df" "gg") "h" ("e" "cc" "abc"))]
    (= (calc-length t1)
       '("a" 10 ("bc" 4 ("df" 2) ("gg" 2)) ("h" 1) ("e" 5 ("cc" 2) ("abc" 3))))
    ))
