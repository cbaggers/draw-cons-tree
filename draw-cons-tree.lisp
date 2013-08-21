;; Ported from scheme to common lisp
;; http://www.t3x.org/s9fes/draw-tree.scm.html
;; It was in the public domain before so it will stay that way now.
(in-package #:draw-cons-tree)

(defparameter *nothing* (cons 'N nil))
(defparameter *visited* (cons 'V nil))

(defun mark-visited (x) (cons *visited* x))
(defun members-of-x (x) (cdr x))

;;------------------------------------------------------

(defun emptyp (x) (equal x *nothing*))

(defun visitedp (x) (equal (car x) *visited*))

(defun donep (x) 
  (and (consp x) (visitedp x) (null (cdr x))))

(defun all-verticalp (n)
  (or (not (consp n))
      (and (null (cdr n))
           (all-verticalp (car n)))))

;;------------------------------------------------------

(defun skip-empty (n)
  (if (and (consp n) (or (emptyp (car n)) (donep (car n))))
      (skip-empty (cdr n))
      n))

(defun remove-trailing-nothing (n)
  (reverse (skip-empty (reverse n))))

;;------------------------------------------------------

(defun draw-tree (n)
  (labels ((%draw-tree (n)
             (when (not (donep n))
               (format t "~%")
               (draw-bars n)
               (format t "~%")
               (%draw-tree (draw-members n)))))
    (if (not (consp n))
        (draw-atom n)
        (%draw-tree (mark-visited (draw-conses n))))
    (format t "~%")))

;;------------------------------------------------------

(defun draw-fixed-string (s) 
  (let* ((b (make-string 8 :initial-element #\space))
         (k (length s))
         (s (if (> k 7) (subseq s 0 7) s))
         (s (if (< k 3) (concatenate 'string " " s) s))
         (k (length s)))
    (format t (concatenate 'string s (subseq b 0 (- 8 k))))))

(defun draw-atom (n) (draw-fixed-string (format nil "~s" n)))

(defun draw-conses (n &optional r)
  (cond ((not (consp n)) (draw-atom n) (reverse r))
        ((null (cdr n)) (format t "[o|/]") (reverse (cons (car n) r)))
        (t (format t "[o|o]---") (draw-conses (cdr n) (cons (car n) r)))))

(defun draw-bars (n)
  (labels ((%draw-bars (n)
             (cond ((not (consp n)))
                   ((emptyp (car n)) (draw-fixed-string "") (%draw-bars (cdr n))) 
                   ((and (consp (car n)) (visitedp (car n)))
                    (%draw-bars (members-of-x (car n))) (%draw-bars (cdr n)))
                   (t (draw-fixed-string "|") (%draw-bars (cdr n))))))
    (%draw-bars (members-of-x n))))

(defun draw-members (n)
  (labels ((%draw-members (n r)
             (cond ((not (consp n)) (mark-visited
                                     (remove-trailing-nothing (reverse r))))
                   ((emptyp (car n)) (draw-fixed-string "")
                    (%draw-members (cdr n) (cons *nothing* r)))
                   ((not (consp (car n))) (draw-atom (car n))
                    (%draw-members (cdr n) (cons *nothing* r)))
                   ((null (cdr n))
                    (%draw-members (cdr n) (cons (draw-final (car n)) r)))
                   ((all-verticalp (car n)) (draw-fixed-string "[o|/]")
                    (%draw-members (cdr n) (cons (caar n) r)))
                   (t (draw-fixed-string "|")
                      (%draw-members (cdr n) (cons (car n) r))))))
    (%draw-members (members-of-x n) nil)))

(defun draw-final (n)
  (cond ((not (consp n)) (draw-atom n) *nothing*)
        ((visitedp n) (draw-members n))
        (t (mark-visited (draw-conses n)))))

;;------------------------------------------------------
