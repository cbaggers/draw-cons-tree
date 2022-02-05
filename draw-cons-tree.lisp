;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DRAW-CONS-TREE
;;;;
;;;; © 2009-2012 Nils M. Holm
;;;; © 2013 Chris Bagley
;;;; © 2022 Michał "phoe" Herda

(defpackage #:draw-cons-tree
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:draw-tree))

(in-package #:draw-cons-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

;; If NIL, the width of every column is adjusted automatically based on the
;; contents of that column. If true, it must be an (INTEGER 5) and denotes the
;; fixed width of all columns. Atoms which cannot fit in a column will be
;; truncated via DRAW-ATOM.
(defvar *cell-width* nil)

;; If NIL, no circularity detection takes place. If true, must be an EQ hash
;; table, and contains a mapping from visited conses to the coordinates where
;; they were first encountered in the tree, in form of (Y X).
(defvar *visited-conses* nil)

;; Counter for reader labels when performing circularity detection; increased
;; by one every time a new label is required.
(defvar *label-counter*)

;; The output stream that receives all drawing activity. :VERTICAL produces
;; output which has more rows, whereas :HORIZONTAL - more columns.
;; Defaults to :VERTICAL.
(defvar *drawing-mode* :vertical)

;; Describes whether the current cell is being drawn with a connection from the
;; left (:LEFT) or from above (:ABOVE). Defaults to :ABOVE.
(defvar *connection-direction* :left)

;; The stream that all drawing output is directed to.
(defvar *draw-output*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing primitives

(defun draw-atom (form)
  (flet ((unreadablep (string)
           (and (string= "#<" (subseq string 0 2))
                (string= ">" (subseq string (1- (length string)))))))
    (let* ((string (prin1-to-string form)))
      ;; Guard against line breaks.
      (nsubstitute #\Space #\Newline string)
      ;; Maybe fix longer strings.
      (cond
        ;; Tiny objects connected from above: append a space to the front.
        ((and (= 1 (length string)) (eq *connection-direction* :above))
         (setf string (format nil " ~A" string)))
        ;; No width limitation: do nothing.
        ((null *cell-width*))
        ;; Short objects: do nothing.
        ((< (length string) *cell-width*))
        ;; Strings:       "..."
        ((stringp form) (setf string "\"...\""))
        ;; Vectors:       #(...)
        ((vectorp form) (setf string "#(..)"))
        ;; Arrays:        #A(...)
        ((arrayp form) (setf string "#A..."))
        ;; Unreadables:   #<...>
        ((unreadablep string) (setf string "#<..>"))
        ;; Otherwise      FOOB...
        (t (let ((width (- *cell-width* 4)))
             (setf string (format nil "~A..." (subseq string 0 width))))))
      string)))

;; A cons cell is in a form of [x|x] where X are one of:
;; * / - if that slot is null,
;; * o - if that slot not null.
(defun draw-cons (form)
  (format nil "[~:[/~;o~]|~:[/~;o~]]" (car form) (cdr form)))

;; One or more vertical lines are drawn every time a CAR needs to be referenced.
;; It has a space before it, so that it lines up with the CAR of the drawn
;; cons cell, like this:
;;   [o|/]
;;    |
;;   ...
(defun draw-vertical ()
  (format nil " |"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities

;; A utility for printing to the draw-output stream.
(defun princd (thing) (princ thing *draw-output*))

(defun fresh-lined () (fresh-line *draw-output*))

;; Make a fresh string containing only spaces. Useful for padding output that is
;; in the middle of the drawing.
(defun make-empty-string (n) (make-string n :initial-element #\Space))

;; Used as the "background" when drawing cons cells with non-empty CDRs.
(defun make-line-string (n) (make-string n :initial-element #\-))

;; Like LIST-LENGTH, except it handles improper lists.
;; * For dotted lists, returns the length of the proper part + 1.
;; * For cyclic lists, returns the length of the acyclic part + 1.
(defun list-length* (form)
  (if (null form)
      0
      (loop with memo = (make-hash-table :test #'eq)
            for result from 1
            for cons = form then (cdr cons)
            if (gethash cons memo)
              return result
            else
              do (setf (gethash cons memo) t)
            when (null (cdr cons))
              return result
            when (and *visited-conses* (gethash form *visited-conses*))
              return (1+ result)
            when (atom (cdr cons))
              return (1+ result))))

;; Like LIST-LENGTH, except it descends into CARs rather than CDRs.
;; * For CAR-dotted lists, returns the height of the proper part + 1.
;; * For CAR-cyclic lists, returns the height of the acyclic part + 1.
(defun list-height* (form)
  (if (null form)
      0
      (loop with memo = (make-hash-table :test #'eq)
            for result from 1
            for cons = form then (car cons)
            if (gethash cons memo)
              return result
            else
              do (setf (gethash cons memo) t)
            when (null (car cons))
              return result
            when (and *visited-conses* (gethash form *visited-conses*))
              return (1+ result)
            when (atom (car cons))
              return (1+ result))))

;; Returns the amount of characters required to write a non-negative integer
;; in base 10.
(defun integer-width (integer)
  (if (zerop integer) 1 (values (ceiling (log (1+ integer) 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Point and canvas

;; A point on the canvas, representing a single slot where a Lisp object
;; can be drawn.
(defstruct point
  ;; The printed representation of the object.
  (body "")
  ;; The reader label associated with that object, or NIL.
  (label nil)
  ;; A boolean stating if the CAR of the object should be printed below.
  (below nil)
  ;; A boolean stating if the CDR of the object should be printed to the right.
  (right nil))

;; Makes an empty canvas, which is an EQUAL hash table whose keys are
;; coordinates like (Y X).
(defun make-canvas () (make-hash-table :test #'equal))

;; Interns a point into a canvas and returns it.
(defun intern-point (canvas y x)
  (let ((key (list y x)))
    (values (a:ensure-gethash key canvas (make-point)))))

;; Returns a point and optionally signals an error if it isn't found.
(defun canvas-point (canvas y x &optional (errorp t))
  (let ((key (list y x)))
    (multiple-value-bind (value foundp) (gethash key canvas)
      (cond (foundp value)
            (errorp (error "Point at (~D ~D) not found." y x))
            (t nil)))))

;; Is a point present in the canvas?
(defun canvas-point-p (canvas y x)
  (let ((key (list y x)))
    (nth-value 1 (gethash key canvas))))

;; Returns the total dimensions of the canvas.
(defun canvas-dimensions (canvas)
  (loop for (y x) being the hash-key of canvas
        maximize x into width
        maximize y into height
        finally (return (values (1+ height) (1+ width)))))

;; Returns a hash table from column numbers to label widths.
;; Only columns which have non-empty labels are included.
(defun canvas-label-column-widths (canvas)
  (let ((result (make-hash-table)))
    (flet ((frob (key value)
             (let ((column (second key)))
               (a:when-let ((label (point-label value)))
                 (let ((length (gethash column result 3)))
                   (setf (gethash column result)
                         (max length (+ 2 (integer-width label)))))))))
      (maphash #'frob canvas))
    result))

;; Returns a hash table from column numbers to column widths.
;; All columns are included.
(defun canvas-body-column-widths (canvas)
  (let ((result (make-hash-table)))
    (flet ((frob (key value)
             (let ((column (second key))
                   (minimum (ecase *drawing-mode*
                              (:vertical 6)
                              (:horizontal 1))))
               (a:when-let ((body (point-body value)))
                 (let ((existing-length (gethash column result minimum)))
                   (setf (gethash column result)
                         (max existing-length (1+ (length body)))))))))
      (maphash #'frob canvas))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Canvas fillers

;; Fills an atom at a canvas point.
(defun fill-canvas-atom (canvas form y x)
  (let ((point (intern-point canvas y x)))
    (setf (point-body point) (draw-atom form))))

;; Fills a reader label at a canvas point.
(defun fill-canvas-reader-label (canvas form y x)
  ;; Get the position of the previous cell.
  (let* ((position (gethash form *visited-conses*))
         ;; Get the previous cell's label.
         (previous (apply #'canvas-point canvas position))
         (label (point-label previous)))
    ;; Was the previous point already referred to?
    (when (null label)
      ;; If not, assign it a new label.
      (setf label (incf *label-counter*)
            (point-label previous) label))
    ;; Fill the reader label.
    (let ((point (intern-point canvas y x)))
      (setf (point-body point) (format nil "#~D#" label)))))

;; Fills a cons cell at a canvas point.
;; Optionally fills its CDR to the right and its CAR below it.
;; In case of :VERTICAL drawing mode, the CDR is filled first.
;; In case of :HORIZONTAL drawing mode, the CAR is filled first.
(defun fill-canvas-cons (canvas form y x)
  ;; Memorize the cons cell.
  (when *visited-conses*
    (setf (gethash form *visited-conses*) (list y x)))
  ;; Fill the current cons cell.
  (let ((point (intern-point canvas y x)))
    (setf (point-body point) (draw-cons form))
    (flet ((fill-cdr ()
             ;; Is the cons's CDR null?
             (unless (null (cdr form))
               ;; No - fill it.
               (setf (point-right point) t)
               (let ((*connection-direction* :left))
                 (fill-canvas canvas (cdr form) y (1+ x)))))
           (fill-car ()
             ;; Is the cons's CAR null?
             (unless (null (car form))
               ;; No - fill it.
               (setf (point-below point) t)
               (let ((*connection-direction* :above))
                 (fill-canvas canvas (car form) (1+ y) x)))))
      (ecase *drawing-mode*
        (:vertical (fill-cdr) (fill-car))
        (:horizontal (fill-car) (fill-cdr))))))

;; Fills a vertical connector at a canvas point and attempts to fill
;; the same form one space below.
(defun fill-canvas-vertical (canvas form y x)
  (let ((point (intern-point canvas y x)))
    (setf (point-body point) (draw-vertical)
          (point-below point) t)
    (fill-canvas canvas form (1+ y) x)))

;; Fills a horizontal connector at a canvas point and attempts to fill
;; the same form one space to the right.
(defun fill-canvas-horizontal (canvas form y x)
  (let ((point (intern-point canvas y x)))
    (setf (point-right point) t)
    (fill-canvas canvas form y (1+ x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filling the canvas

;; Fills the canvas based on the provided form.
;; This is a mutually recursive function that descends into CDRs first
(defun fill-canvas (canvas form y x)
  (labels ((cons-previously-visited-p (form)
             ;; If we are handling circularities...
             (when *visited-conses*
               ;; ...have we already visited that cons cell?
               (nth-value 1 (gethash form *visited-conses*))))
           (enough-width-p (canvas form y x)
             (or
              ;; Can we draw a reader label?
              (and *visited-conses* (gethash form *visited-conses*))
              ;; If not, are there enough free points to the right for us
              ;; to fit our target list in there without overwriting anything?
              (loop with length = (list-length* form)
                    for new-x from x below (+ x length)
                    never (canvas-point-p canvas y new-x))))
           (enough-height-p (canvas form y x)
             (or
              ;; Can we draw a reader label?
              (and *visited-conses* (gethash form *visited-conses*))
              ;; If not, are there enough free points below for us
              ;; to fit our target list in there without overwriting anything?
              (loop with height = (list-height* form)
                    for new-y from y below (+ y height)
                    never (canvas-point-p canvas new-y x)))))
    (cond
      ;; Is the current form an atom?
      ((atom form)
       (fill-canvas-atom canvas form y x))
      ;; Can we draw a reader label?
      ((cons-previously-visited-p form)
       (fill-canvas-reader-label canvas form y x))
      ;; Vertical drawing mode:
      ;; Do we have enough width to draw this list on current height?
      ((and (eq *drawing-mode* :vertical)
            (enough-width-p canvas form y x))
       (fill-canvas-cons canvas form y x))
      ;; Horizontal drawing mode:
      ;; Do we have enough height to draw this list on current height?
      ((and (eq *drawing-mode* :horizontal)
            (enough-height-p canvas form y x))
       (fill-canvas-cons canvas form y x))
      ;; Vertical drawing mode:
      ;; No space; draw a vertical line and try again below.
      ((eq *drawing-mode* :vertical)
       (fill-canvas-vertical canvas form y x))
      ;; Horizontal drawing mode:
      ;; No space; draw a horizontal line and try again below.
      ((eq *drawing-mode* :horizontal)
       (fill-canvas-horizontal canvas form y x))))
  canvas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing the canvas

(defun draw-main-line (canvas width y prefix
                       label-column-widths body-column-widths)
  ;; Compute the number of non-empty columns in the row.
  (let ((real-width (loop for x from width downto 0
                          when (canvas-point-p canvas y (1- x))
                            return x)))
    (unless (= y 0) (fresh-lined))
    (when prefix (princd prefix))
    ;; For all columns...
    (dotimes (x real-width)
      (let* ((point (or (canvas-point canvas y x nil) (make-point))))
        ;; Draw the label, if any.
        (a:when-let ((label-column-width (gethash x label-column-widths 0)))
          (let ((label (point-label point)))
            (cond (label
                   (let ((label (format nil "#~D=" label)))
                     (princd (format nil "~v@A" label-column-width label))))
                  ((and (canvas-point-p canvas y (1- x))
                        (point-right (canvas-point canvas y (1- x))))
                   (princd (make-line-string label-column-width)))
                  (t
                   (princd (make-empty-string label-column-width))))))
        ;; Draw the body.
        (let ((body-width (or *cell-width* (gethash x body-column-widths)))
              (body (point-body point))
              (right (point-right point)))
          (cond (right
                 (princd (format nil "~v,,,'-A" body-width body)))
                ((= x (1- real-width))
                 (princd body))
                (t
                 (princd (format nil "~vA" body-width body)))))))))

(defun draw-below-line (canvas width y prefix
                        label-column-widths body-column-widths)
  ;; Compute the number of non-empty columns in the row.
  (a:when-let ((real-width (loop for x from width downto 0
                                 when (and (canvas-point-p canvas y (1- x))
                                           (point-below
                                            (canvas-point canvas y (1- x))))
                                   return x)))
    (fresh-lined)
    (when prefix (princd prefix))
    ;; For all columns...
    (dotimes (x real-width)
      (let* ((point (or (canvas-point canvas y x nil) (make-point))))
        ;; Draw the empty column width.
        (a:when-let ((label-column-width (gethash x label-column-widths 0)))
          (princd (make-empty-string label-column-width)))
        ;; Draw the vertical, if any.
        (let ((body-width (or *cell-width* (gethash x body-column-widths)))
              (below (point-below point))
              (vertical (draw-vertical)))
          (cond ((not below)
                 (princd (make-empty-string body-width)))
                ((= x (1- real-width))
                 (princd vertical))
                (t
                 (princd (format nil "~vA" body-width vertical)))))))))

(defun draw-canvas (canvas &optional prefix)
  ;; Compute the label and body column widths.
  (let ((label-column-widths (canvas-label-column-widths canvas))
        (body-column-widths (canvas-body-column-widths canvas)))
    ;; Compute the canvas dimensions.
    (multiple-value-bind (height width) (canvas-dimensions canvas)
      ;; For all rows...
      (dotimes (y height)
        ;; Draw the main line.
        (draw-main-line canvas width y prefix
                        label-column-widths body-column-widths)
        ;; Draw the below-line.
        (draw-below-line canvas width y prefix
                         label-column-widths body-column-widths)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

;; This function works in two phases:
;; * The first phase creates a "canvas" and populates it with points
;;   representing the conses and atoms of the original form.
;; * The second phase iterates line by line through the canvas and prints its
;;   contents to a stream.
;; The canvas is a sparse data structure that should be easily resizable.
;; It is currently implemented as a hash-table.
(defun %draw-tree (form width prefix drawing-mode)
  ;; Make a fresh canvas and bind *CELL-WIDTH* and *DRAWING-MODE*.
  (let* ((canvas (make-hash-table :test #'equal))
         (*cell-width* width)
         (*drawing-mode* drawing-mode))
    ;; Do we handle circularity?
    (if *print-circle*
        ;; Yes - bind *VISITED-CONSES* and *LABEL-COUNTER*.
        (let ((*visited-conses* (make-hash-table :test #'eq))
              (*label-counter* 0))
          (fill-canvas canvas form 0 0))
        ;; No - fill the canvas without circularity detection.
        (fill-canvas canvas form 0 0))
    ;; Print the filled canvas.
    (draw-canvas canvas prefix)))

(defun draw-tree (form &key
                         width
                         (prefix ";;; ")
                         (stream t)
                         (drawing-mode :vertical))
  "Draws a cons tree in ASCII-art style.
\
If *PRINT-CIRCLE* is true, structure sharing is detected in the printed tree.
\
Keyword arguments:
* WIDTH - if NIL, the width of every column is adjusted automatically based on
          the contents of that column. If true, it must be an (INTEGER 5) and
          denotes the fixed width of all columns. Atoms which cannot fit in a
          column will be truncated. Defaults to NIL.
* PREFIX - the prefix that will be printed before every line, or NIL for no
           prefix. Defaults to \";;; \".
* STREAM - the stream that the form will be drawn to, or T for standard output,
           or NIL if a string should be returned. Defaults to T.
* DRAWING-MODE - denotes the preferred direction of printing the cons tree.
                 :VERTICAL produces output which has more rows, whereas
                 :HORIZONTAL - more columns. Defaults to :VERTICAL."
  ;; Check provided argument types.
  (check-type prefix (or null string))
  (check-type stream (or boolean stream))
  (check-type drawing-mode (member :vertical :horizontal))
  (check-type width (or null (integer 5)))
  ;; Handle :STREAM.
  (cond
    ;; T - print to standard output (default).
    ((eq stream t)
     (let ((*draw-output* *standard-output*))
       (%draw-tree form width prefix drawing-mode)))
    ;; NIL - gather all output into a string and return it.
    ((eq stream nil)
     (with-output-to-string (*draw-output*)
       (%draw-tree form width prefix drawing-mode)))
    ;; A stream - rebind the standard output.
    (t (let ((*draw-output* stream))
         (%draw-tree form width prefix drawing-mode)))))

;;; Tests

(defmacro test (form string)
  (let ((string (subseq string 1)))
    `(assert (string= ,string (with-output-to-string (*standard-output*)
                                (draw-cons-tree:draw-tree ',form))))))
