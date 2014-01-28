;;;; tests.lisp

(in-package #:ascii-tree)

(defvar smallt '("a" ("bc" "df" "gg") "h" ("e" "cc" "abc")))

(defvar bigt
  '("a" ("bc" ("df" "v" "vaca") ("gg" "123")) ("h" "helo" "ola") ("e" ("abaccaa" "w") ("abc" "ah" "fff" "hi" "aabb"))))

(defun report (result form)
  "Prints FAIL/Pass for the form on the screen if the result is true/false."
  (format t "~:[FAIL~;Pass~] ... ~a~%" result form))

(defmacro ok? (form)
  "Simplifies the usage of the fn report."
  `(report ,form ',form))

(defun info-tests ()
  (ok? (= 3 (height smallt)))
  (ok? (= 20 (width smallt 2)))
  (ok? (= 4 (height bigt)))
  (ok? (= 53 (width bigt 2)))
  (let ((elem "ola") (c #\_)) 
    (ok? (string= "ola" (fill-str elem 3 c)))
    (ok? (string= " ola" (fill-str elem 4 c)))
    (ok? (string= " ola " (fill-str elem 5 c)))
    (ok? (string= " _ola " (fill-str elem 6 c)))
    (ok? (string= " _ola_ " (fill-str elem 7 c)))
    (ok? (string= " __ola_ " (fill-str elem 8 c))))
  (ok? (equalp (uniform-width '("e" ("cc" "w") "abc") 2)
               '("e" ("cc" " w") "abc"))))

(defun print-tests ()
  (format t "~a~%" smallt)
  (print-tree! smallt)
  (print-tree! smallt 1)
  (format t "~a~%" bigt)
  (print-tree! bigt)
  (print-tree! bigt 4))
