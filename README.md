# DRAW-CONS-TREE

Draws a cons tree in ASCII-art style.

* Originally created by Nils M. Holm as a Scheme program in the function library of ["Scheme 9 from Empty Space"](http://www.t3x.org/s9fes/draw-tree.scm.html);
* Ported to Common Lisp by Chris Bagley;
* Extended and tested by Michał "phoe" Herda.

## API

* Function **`DRAW-TREE`**
  * `(draw-tree form &key width prefix stream drawing-mode)`
  * Draws a cons tree in ASCII-art style. If `*PRINT-CIRCLE*` is true, structure sharing is detected in the printed tree.
  * Keyword arguments:
    * **`:WIDTH`** - if `NIL`, the width of every column is adjusted automatically based on the contents of that column. If true, it must be an `(INTEGER 5)` and denotes the fixed width of all columns. Atoms which cannot fit in a column will be truncated. Defaults to `NIL`.
    * **`:PREFIX`** - the prefix that will be printed before every line, or `NIL` for no prefix. Defaults to `";;; "`.
    * **`:STREAM`** - the stream that the form will be drawn to, or `T` for standard output, or `NIL` if a string should be returned. Defaults to `T`.
    * **`:DRAWING-MODE`** - denotes the preferred direction of printing the cons tree. `:VERTICAL` produces output which has more rows, whereas `:HORIZONTAL` - more columns. Defaults to `:VERTICAL`.

## Examples

### General use

The entry point is the function `DRAW-TREE` which will attempt to draw any cons tree provided to it to standard output.

```lisp
DRAW-CONS-TREE> (draw-tree '(foo bar baz))
;;; [o|o]-[o|o]-[o|/]
;;;  |     |     |
;;; FOO   BAR   BAZ
NIL

DRAW-CONS-TREE> (draw-tree `(defun foo (&rest things)
                              (apply #'+ things)))
;;; [o|o]-[o|o]-[o|o]-[o|/]
;;;  |     |     |     |
;;; DEFUN FOO    |    [o|o]--[o|o]----[o|/]
;;;              |     |      |        |
;;;              |    APPLY   |       THINGS
;;;              |            |
;;;             [o|o]-[o|/]  [o|o]----[o|/]
;;;              |     |      |        |
;;;             &REST THINGS FUNCTION  +
NIL
```

### Fixed width

By default, the width of each column is determined automatically. It is possible to provide fixed-width output by supplying the `WIDTH` keyword argument. Atoms whose name cannot fit are automatically truncated.

```lisp
DRAW-CONS-TREE> (draw-tree `(defun foo (&rest things)
                              (apply #'+ things))
                           :width 5)
;;; [o|o][o|o][o|o][o|/]
;;;  |    |    |    |
;;; D... FOO   |   [o|o][o|o][o|/]
;;;            |    |    |    |
;;;            |   A...  |   T...
;;;            |         |
;;;           [o|o][o|/][o|o][o|/]
;;;            |    |    |    |
;;;           &... T... F...  +
NIL

DRAW-CONS-TREE> (draw-tree `(defun foo (&rest things)
                              (apply #'+ things))
                           :width 12)
;;; [o|o]-------[o|o]-------[o|o]-------[o|/]
;;;  |           |           |           |
;;; DEFUN       FOO          |          [o|o]-------[o|o]-------[o|/]
;;;                          |           |           |           |
;;;                          |          APPLY        |          THINGS
;;;                          |                       |
;;;                         [o|o]-------[o|/]       [o|o]-------[o|/]
;;;                          |           |           |           |
;;;                         &REST       THINGS      FUNCTION     +
NIL
```

### Custom prefix

It's possible to supply a custom per-line prefix via the `PREFIX` keyword argument:

```lisp
DRAW-CONS-TREE> (draw-tree '(foo bar baz)
                           :prefix ">>> ")
>>> [o|o]-[o|o]-[o|/]
>>>  |     |     |
>>> FOO   BAR   BAZ
NIL
```

### Stream selection

It's possible to select the stream that will be drawn to via the `:STREAM` keyword argument, which also accepts `T` and `NIL` the same way `FORMAT` does:

```lisp
DRAW-CONS-TREE> (draw-tree '(foo bar baz)
                           :stream t)
;;; [o|o]-[o|o]-[o|/]
;;;  |     |     |
;;; FOO   BAR   BAZ
NIL

DRAW-CONS-TREE> (with-output-to-string (s)
                  (draw-tree '(foo bar baz)
                             :stream s))
";;; [o|o]-[o|o]-[o|/]
;;;  |     |     |
;;; FOO   BAR   BAZ"

DRAW-CONS-TREE> (draw-tree '(foo bar baz)
                           :stream nil)
";;; [o|o]-[o|o]-[o|/]
;;;  |     |     |
;;; FOO   BAR   BAZ"
```

### Drawing mode

It is possible to request a preferred direction of printing the cons tree via the `DRAWING-MODE` keyword argument. `:VERTICAL` produces output which has more rows, whereas `:HORIZONTAL` - more columns.

```lisp
DRAW-CONS-TREE/TEST> (draw-tree '((1 2 3) (4 5 6) (7 8 9)) 
                                :drawing-mode :vertical)
;;; [o|o]-[o|o]-[o|/]
;;;  |     |     |
;;;  |     |    [o|o]-[o|o]-[o|/]
;;;  |     |     |     |     |
;;;  |     |     7     8     9
;;;  |     |
;;;  |    [o|o]-[o|o]-[o|/]
;;;  |     |     |     |
;;;  |     4     5     6
;;;  |
;;; [o|o]-[o|o]-[o|/]
;;;  |     |     |
;;;  1     2     3
NIL

DRAW-CONS-TREE/TEST> (draw-tree '((1 2 3) (4 5 6) (7 8 9)) 
                                :drawing-mode :horizontal)
;;; [o|o]-------------[o|o]-------------[o|/]
;;;  |                 |                 |
;;; [o|o]-[o|o]-[o|/] [o|o]-[o|o]-[o|/] [o|o]-[o|o]-[o|/]
;;;  |     |     |     |     |     |     |     |     |
;;;  1     2     3     4     5     6     7     8     9
NIL
```

The horizontal mode is sort of useful for visualizing Lisp code.

```lisp
DRAW-CONS-TREE/TEST> (draw-tree `(defun foo (&rest things)
                                   (apply #'+ things))
                                :drawing-mode :horizontal)
;;; [o|o]-[o|o]-[o|o]--------[o|/]
;;;  |     |     |            |
;;; DEFUN FOO   [o|o]-[o|/]  [o|o]-[o|o]----------[o|/]
;;;              |     |      |     |              |
;;;             &REST THINGS APPLY [o|o]----[o|/] THINGS
;;;                                 |        |
;;;                                FUNCTION  +
NIL
```

### Circularity handling

`DRAW-TREE` respects `*PRINT-CIRCLE*` and will detect circularities and shared structure if that variable is true.

```lisp
DRAW-CONS-TREE> (let ((*print-circle* t))
                  (draw-tree '#1=(#1# . nil)))
;;; #1=[o|/]
;;;     |
;;;    #1#
NIL

DRAW-CONS-TREE> (let ((*print-circle* t))
                  (draw-tree '#1=(nil . #1#)))
;;; #1=[/|o]-#1#
NIL

DRAW-CONS-TREE> (let ((*print-circle* t))
                  (draw-tree '#1=(#1# . #1#)))
;;; #1=[o|o]-#1#
;;;     |
;;;    #1#
NIL

DRAW-CONS-TREE> (let ((*print-circle* t))
                  (draw-tree '(#1=(1 2 3) #1#
                               #2=(2 3 4) #2#)))
;;; [o|o]----[o|o]-[o|o]----[o|/]
;;;  |        |     |        |
;;; #2#       |    #1#   #1=[o|o]-[o|o]-[o|/]
;;;           |              |     |     |
;;;           |              2     3     4
;;;           |
;;;       #2=[o|o]-[o|o]----[o|/]
;;;           |     |        |
;;;           1     2        3
NIL
```

## Testing

`(asdf:test-system :draw-cons-tree)`

## License

Public domain (just like the original).
