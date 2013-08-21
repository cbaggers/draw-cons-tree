draw-cons-tree
==============

Draws and ascii picture of a cons tree.

    CL-USER> (draw-tree '(a b (c nil 1)))
    [o|o]---[o|o]---[o|/]
     |       |       |      
     A       B      [o|o]---[o|o]---[o|/]
                     |       |       |      
                     C      NIL      1      
    NIL

Ported from the function library of "Scheme 9 from Empty Space"
    http://www.t3x.org/s9fes/draw-tree.scm.html

Library is in the public domain in keeping with the original
