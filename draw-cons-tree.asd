;;;; draw-cons-tree.asd

(asdf:defsystem #:draw-cons-tree
  :serial t
  :description "Makes and ascii picture of a cons tree"
  :author "Ported by:CBaggers - Original Author:Nils M Holm"
  :license "Public Domain"
  :components ((:file "package")
               (:file "draw-cons-tree")))

