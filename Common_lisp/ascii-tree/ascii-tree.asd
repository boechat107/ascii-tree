;;;; ascii-tree.asd

(asdf:defsystem #:ascii-tree
  :serial t
  :description "Describe ascii-tree here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:queues.simple-queue)
  :components ((:file "package")
               (:file "ascii-tree")
               (:file "tests")))

