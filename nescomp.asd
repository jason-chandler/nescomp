;;;; nescomp.asd

(asdf:defsystem #:nescomp
  :description "Describe nescomp here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "comp")
               (:file "sexp-asm")
               (:file "nescomp")))
