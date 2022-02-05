;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DRAW-CONS-TREE
;;;;
;;;; © 2009-2012 Nils M. Holm
;;;; © 2013 Chris Bagley
;;;; © 2022 Michał "phoe" Herda

(asdf:defsystem #:draw-cons-tree
  :author ("Nils M. Holm"
           "Chris Bagley"
           "Michał \"phoe\" Herda <phoe@disroot.org>")
  :mailto "phoe@disroot.org"
  :description "Draws a cons tree in ASCII-art style."
  :license "Public Domain"
  :version "1.0"
  :depends-on (#:alexandria)
  :components ((:file "draw-cons-tree"))
  :serial t
  :in-order-to ((test-op (load-op #:draw-cons-tree/test)))
  :perform (test-op (op c)
             (uiop:symbol-call '#:5am '#:run!
                               (find-symbol (symbol-name '#:draw-cons-tree)
                                            '#:draw-cons-tree/test))))

(defsystem #:draw-cons-tree/test
  :author ("Michał \"phoe\" Herda <phoe@disroot.org>")
  :mailto "phoe@disroot.org"
  :description "test system for draw-cons-tree"
  :license "Public Domain"
  :version "1.0"
  :depends-on (#:draw-cons-tree
               #:split-sequence
               #:fiveam)
  :components ((:file "draw-cons-tree-test"))
  :serial t)
