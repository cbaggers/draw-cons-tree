;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DRAW-CONS-TREE
;;;;
;;;; © 2009-2012 Nils M. Holm
;;;; © 2013 Chris Bagley
;;;; © 2022 Michał "phoe" Herda

(defpackage #:draw-cons-tree/test
  (:use #:cl
        #:draw-cons-tree
        #:fiveam)
  (:import-from #:split-sequence #:split-sequence)
  (:export #:draw-cons-tree))

(in-package #:draw-cons-tree/test)

(def-suite draw-cons-tree)
(in-suite draw-cons-tree)

(test stream
  (let ((expected (with-output-to-string (*standard-output*)
                    (draw-tree '(nil) :stream t))))
    (is (string= ";;; [/|/]" expected)))
  (let ((expected (with-output-to-string (s)
                    (draw-tree '(nil) :stream s))))
    (is (string= ";;; [/|/]" expected)))
  (let ((expected (draw-tree '(nil) :stream nil)))
    (is (string= ";;; [/|/]" expected))))

(defun prepare-expected (string)
  (let ((lines (split-sequence #\Newline string :remove-empty-subseqs t)))
    (flet ((trim (string) (string-left-trim '(#\Space) string)))
      (format nil "~{~A~^~%~}" (mapcar #'trim lines)))))

(defmacro test-drawing ((form &rest args) &body (expected))
  `(let ((actual (with-output-to-string (s)
                   (draw-tree ,form ,@args :stream s)))
         (expected (prepare-expected ,expected)))
     (is (string= expected actual))))

(test prefix
  (test-drawing ('(nil) :prefix ";;; ")
    ";;; [/|/]")
  (test-drawing ('(nil) :prefix ">>> ")
    ">>> [/|/]"))

(test draw-atoms
  ;; Symbols.
  (test-drawing ('a)
    ";;; A")
  (test-drawing ('nil)
    ";;; NIL")
  (test-drawing (':test)
    ";;; :TEST")
  (test-drawing ('&optional)
    ";;; &OPTIONAL")
  (test-drawing ('&optional :width 5)
    ";;; &...")
  ;; Numbers.
  (test-drawing (1234567890)
    ";;; 1234567890")
  (test-drawing (1234567890 :width 5)
    ";;; 1...")
  ;; Strings.
  (test-drawing ("foobarbaz")
    ";;; \"foobarbaz\"")
  (test-drawing ("foobarbaz" :width 5)
    ";;; \"...\"")
  ;; Vectors.
  (test-drawing (#(1 2 3 4 5))
    ";;; #(1 2 3 4 5)")
  (test-drawing (#(1 2 3 4 5) :width 5)
    ";;; #(..)")
  ;; Arrays.
  (test-drawing (#2A((1 2 3 4 5)))
    ";;; #2A((1 2 3 4 5))")
  (test-drawing (#2A((1 2 3 4 5)) :width 5)
    ";;; #A...")
  ;; Unreadables.
  (test-drawing (#'car)
    (format nil ";;; ~A" #'car))
  (test-drawing (#'car :width 5)
    ";;; #<..>"))

(test draw-simple-conses
  (test-drawing ('(nil . nil))
    ";;; [/|/]")
  (test-drawing ('(nil nil))
    ";;; [/|o]-[/|/]")
  (test-drawing ('(nil nil nil nil))
    ";;; [/|o]-[/|o]-[/|o]-[/|/]")
  (test-drawing ('((nil) . nil))
    ";;; [o|/]
     ;;;  |
     ;;; [/|/]")
  (test-drawing ('((nil) (nil)))
    ";;; [o|o]-[o|/]
     ;;;  |     |
     ;;; [/|/] [/|/]")
  (test-drawing ('(((((nil))))))
    ";;; [o|/]
     ;;;  |
     ;;; [o|/]
     ;;;  |
     ;;; [o|/]
     ;;;  |
     ;;; [o|/]
     ;;;  |
     ;;; [/|/]")
  (test-drawing ('((nil nil nil) (nil nil) (nil)))
    ";;; [o|o]-[o|o]-[o|/]
     ;;;  |     |     |
     ;;;  |     |    [/|/]
     ;;;  |     |
     ;;;  |    [/|o]-[/|/]
     ;;;  |
     ;;; [/|o]-[/|o]-[/|/]")
  (test-drawing ('((nil) (nil nil) (nil nil nil)))
    ";;; [o|o]-[o|o]-[o|/]
     ;;;  |     |     |
     ;;; [/|/]  |    [/|o]-[/|o]-[/|/]
     ;;;        |
     ;;;       [/|o]-[/|/]"))

(test draw-data
  (test-drawing ('(foo bar baz))
    ";;; [o|o]-[o|o]-[o|/]
     ;;;  |     |     |
     ;;; FOO   BAR   BAZ")
  (test-drawing (`(defun foo (&rest things)
                    (apply #'+ things)))
    ";;; [o|o]-[o|o]-[o|o]-[o|/]
     ;;;  |     |     |     |
     ;;; DEFUN FOO    |    [o|o]--[o|o]----[o|/]
     ;;;              |     |      |        |
     ;;;              |    APPLY   |       THINGS
     ;;;              |            |
     ;;;             [o|o]-[o|/]  [o|o]----[o|/]
     ;;;              |     |      |        |
     ;;;             &REST THINGS FUNCTION  +")
  (test-drawing ((read-from-string "(#1=(1 2 3) #1#
                                     #2=(2 3 4) #2#)"))
    ";;; [o|o]-[o|o]-[o|o]-[o|/]
     ;;;  |     |     |     |
     ;;;  |     |     |    [o|o]-[o|o]-[o|/]
     ;;;  |     |     |     |     |     |
     ;;;  |     |     |     2     3     4
     ;;;  |     |     |
     ;;;  |     |    [o|o]-[o|o]-[o|/]
     ;;;  |     |     |     |     |
     ;;;  |     |     2     3     4
     ;;;  |     |
     ;;;  |    [o|o]-[o|o]-[o|/]
     ;;;  |     |     |     |
     ;;;  |     1     2     3
     ;;;  |
     ;;; [o|o]-[o|o]-[o|/]
     ;;;  |     |     |
     ;;;  1     2     3"))

(test print-circle-simple
  (let ((*print-circle* t))
    (test-drawing ((read-from-string "#1=(#1# . nil)"))
      ";;; #1=[o|/]
       ;;;     |
       ;;;    #1#")
    (test-drawing ((read-from-string "#1=(nil . #1#)"))
      ";;; #1=[/|o]-#1#")
    (test-drawing ((read-from-string "#1=(#1# . #1#)"))
      ";;; #1=[o|o]-#1#
       ;;;     |
       ;;;    #1#")))

(test print-circle
  (let ((*print-circle* t))
    (test-drawing ((read-from-string "#1=(1 2 3 . #1#)"))
      ";;; #1=[o|o]-[o|o]-[o|o]-#1#
       ;;;     |     |     |
       ;;;     1     2     3"))
  (let ((*print-circle* t))
    (test-drawing ((read-from-string "(#1=(1 2 3) #1#
                                       #2=(2 3 4) #2#)"))
      ";;; [o|o]----[o|o]-[o|o]----[o|/]
       ;;;  |        |     |        |
       ;;; #2#       |    #1#   #1=[o|o]-[o|o]-[o|/]
       ;;;           |              |     |     |
       ;;;           |              2     3     4
       ;;;           |
       ;;;       #2=[o|o]-[o|o]----[o|/]
       ;;;           |     |        |
       ;;;           1     2        3")))

(test drawing-mode
  (test-drawing ((read-from-string "((1 #1=(2) . 3) #1# 5)")
                 :drawing-mode :vertical)
    ";;; [o|o]-[o|o]-[o|/]
     ;;;  |     |     |
     ;;;  |    [o|/]  5
     ;;;  |     |
     ;;;  |     2
     ;;;  |
     ;;; [o|o]-[o|o]-3
     ;;;  |     |
     ;;;  1    [o|/]
     ;;;        |
     ;;;        2")
  (test-drawing ((read-from-string "((1 #1=(2) . 3) #1# 5)")
                 :drawing-mode :horizontal)
    ";;; [o|o]---------[o|o]-[o|/]
     ;;;  |             |     |
     ;;; [o|o]-[o|o]-3 [o|/]  5
     ;;;  |     |       |
     ;;;  1    [o|/]    2
     ;;;        |
     ;;;        2")
  (test-drawing ((read-from-string "(#1=(1 2 3) #1#)")
                 :drawing-mode :horizontal)
    ";;; [o|o]-------------[o|/]
     ;;;  |                 |
     ;;; [o|o]-[o|o]-[o|/] [o|o]-[o|o]-[o|/]
     ;;;  |     |     |     |     |     |
     ;;;  1     2     3     1     2     3")
  (test-drawing ((read-from-string "(#1=(1 2 3) #1#)")
                 :drawing-mode :vertical)
    ";;; [o|o]-[o|/]
     ;;;  |     |
     ;;;  |    [o|o]-[o|o]-[o|/]
     ;;;  |     |     |     |
     ;;;  |     1     2     3
     ;;;  |
     ;;; [o|o]-[o|o]-[o|/]
     ;;;  |     |     |
     ;;;  1     2     3"))

(test drawing-mode-print-circle
  (let ((*print-circle* t))
    (test-drawing ((read-from-string "((1 #1=(2) . 3) #1# 5)")
                   :drawing-mode :vertical)
      ";;; [o|o]----[o|o]-[o|/]
       ;;;  |        |     |
       ;;;  |    #1=[o|/]  5
       ;;;  |        |
       ;;;  |        2
       ;;;  |
       ;;; [o|o]----[o|o]-3
       ;;;  |        |
       ;;;  1       #1#")
    (test-drawing ((read-from-string "((1 #1=(2) . 3) #1# 5)")
                   :drawing-mode :horizontal)
      ";;; [o|o]------------[o|o]-[o|/]
       ;;;  |                |     |
       ;;; [o|o]----[o|o]-3 #1#    5
       ;;;  |        |
       ;;;  1    #1=[o|/]
       ;;;           |
       ;;;           2")
    (test-drawing ((read-from-string "(#1=(1 2 3) #1#)")
                   :drawing-mode :horizontal)
      ";;;    [o|o]-------------[o|/]
       ;;;     |                 |
       ;;; #1=[o|o]-[o|o]-[o|/] #1#
       ;;;     |     |     |
       ;;;     1     2     3")
    (test-drawing ((read-from-string "(#4=(1 2 3) #4#)")
                   :drawing-mode :vertical)
      ";;; [o|o]----[o|/]
       ;;;  |        |
       ;;; #1#   #1=[o|o]-[o|o]-[o|/]
       ;;;           |     |     |
       ;;;           1     2     3")))
