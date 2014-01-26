(ns ascii-tree.core-test
  (:use clojure.test
        ascii-tree.core))

(def bigt
  '("a" ("bc" ("df" "v" "vaca") ("gg" "123")) ("h" "helo" "ola") ("e" ("abaccaa" "w") ("abc" "ah" "fff" "hi" "aabb"))))

(def smallt '("a" ("bc" "df" "gg") "h" ("e" "cc" "abc")))

(deftest info-tests
  (is (= "  _123_ " (fill-str "123" 8 \_)))
  (is (= "123" (fill-str "123" 3 \_)))
  (is (= " 123" (fill-str "123" 4 \_)))
  (is (= " 123 " (fill-str "123" 5 \_)))
  (= (add-width-info smallt 2)
     '("a" 21 ("bc" 8 ("df" 4) ("gg" 4)) ("h" 4) ("e" 9 ("cc" 4) ("abc" 5))))
  (is (== 3 (height smallt)))
  (is (== 4 (height bigt))))

(deftest print-tests!
  (println bigt)
  (print-tree! bigt)
  (print-tree! bigt 4)
  (newline)
  (println smallt)
  (print-tree! smallt)
  (print-tree! smallt 1))
