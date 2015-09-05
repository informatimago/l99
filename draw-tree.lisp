;;;; -*- coding:utf-8 -*-

(ql:quickload :com.informatimago.common-lisp.cesarum)
(ql:quickload :com.informatimago.common-lisp.picture)


;; ─ ━ │ ┃ ┄ ┅ ┆ ┇ ┈ ┉ ┊ ┋ ┌ ┍ ┎ ┏ ┐ ┑ ┒ ┓ └ ┕ ┖ ┗ ┘ ┙ ┚ ┛ ├ ┝ ┞ ┟ ┠ ┡
;; ┢ ┣ ┤ ┥ ┦ ┧ ┨ ┩ ┪ ┫ ┬ ┭ ┮ ┯ ┰ ┱ ┲ ┳ ┴ ┵ ┶ ┷ ┸ ┹ ┺ ┻ ┼ ┽ ┾ ┿ ╀ ╁ ╂ ╃
;; ╄ ╅ ╆ ╇ ╈ ╉ ╊ ╋ ╌ ╍ ╎ ╏ ═ ║ ╒ ╓ ╔ ╕ ╖ ╗ ╘ ╙ ╚ ╛ ╜ ╝ ╞ ╟ ╠ ╡ ╢ ╣ ╤ ╥
;; ╦ ╧ ╨ ╩ ╪ ╫ ╬ ╭ ╮ ╯ ╰ ╱ ╲ ╳ ╴ ╵ ╶ ╷ ╸ ╹ ╺ ╻ ╼ ╽ ╾ ╿

(defparameter *unicode-box*
  '(:top-left     "╔"
    :top-right    "╗"
    :bottom-left  "╚"
    :bottom-right "╝"
    :top          "═"
    :bottom       "═"
    :left         "║"
    :right        "║"))

(defparameter *unicode-line*
  '(:top-left     "┌"
    :top-right    "┐"
    :bottom-left  "└"
    :bottom-right "┘"
    :horizontal   "─"
    :vertical     "│"
    :bottom-butt  "╧"
    :top-butt     "╤"
    :left-butt    "╟"
    :right-butt   "╢"))


(defparameter *ascii-box*
  '(:top-left     "+"
    :top-right    "+"
    :bottom-left  "+"
    :bottom-right "+"
    :top          "-"
    :bottom       "-"
    :left         "|"
    :right        "|"))

(defparameter *ascii-line*
  '(:top-left     "+"
    :top-right    "+"
    :bottom-left  "+"
    :bottom-right "+"
    :horizontal   "-"
    :vertical     "|"
    :bottom-butt  "-"
    :top-butt     "-"
    :left-butt    "|"
    :right-butt   "|"))


(defparameter *box*  *unicode-box*)
(defparameter *line* *unicode-line*)


(defstruct subtree-view
  "

          +---*
          |   
    +-------+ 
    * label |
    +-------+
          |   
          +---*

The origin is the left of the label box.
"
  node
  label                 ; the label string
  label-box-width
  vertical-height-above ; length of the vertical line above the origin.
  vertical-height-below ; length of the vertical line below the origin.
  width                 ; total width.
  height-above          ; height above origin.
  height-below          ; height below origin.
  left-subtree-view
  right-subtree-view)


(defun subtree-view-height (subtree-view)
  (+ 1 ; one more, for the label.
     (subtree-view-height-above subtree-view)
     (subtree-view-height-below subtree-view)))


(defun binary-tree-to-view (tree)
  (if (binary-tree-empty-p tree)
      (make-subtree-view :node tree
                         :label " nil"
                         :label-box-width 4
                         :vertical-height-above 0
                         :vertical-height-below 0
                         :width 4
                         :height-above 0
                         :height-below 0)
     (let* ((label (princ-to-string (binary-tree-label tree)))
            (view (make-subtree-view :node tree
                                     :label label
                                     :label-box-width (+ 4 (length label))
                                     :right-subtree-view (binary-tree-to-view (binary-tree-right tree))
                                     :left-subtree-view  (binary-tree-to-view (binary-tree-left  tree)))))
       (setf (subtree-view-vertical-height-above view) (1+ (subtree-view-height-below (subtree-view-right-subtree-view view)))
             (subtree-view-height-above view)          (1+ (subtree-view-height       (subtree-view-right-subtree-view view)))
             (subtree-view-vertical-height-below view) (1+ (subtree-view-height-above (subtree-view-left-subtree-view view)))
             (subtree-view-height-below view)          (1+ (subtree-view-height       (subtree-view-left-subtree-view view)))
             (subtree-view-width view)                 (+ (subtree-view-label-box-width view)
                                                          3
                                                          (max (subtree-view-width (subtree-view-left-subtree-view view))
                                                               (subtree-view-width (subtree-view-right-subtree-view view)))))
       view)))


#-(and) "

                           
        5                  ┌─── nil    2
        4             ╔════╧═╗         1
        3         ┌───╢ 6789 ║         0
        2         │   ╚════╤═╝         -1
        1   ╔═════╧═╗      └─── nil    -2
        0   ╢ 12345 ║
        -1  ╚═════╤═╝                  
        -2        └─── nil            0 


                     12345    6789   nil
height-above           5        2     0
vertical-height-above  3        2     0
height-below           2        2     0
vertical-height-below  2        2     0

"

(defun draw-tree-view (view x y pict)
  (if (binary-tree-empty-p (subtree-view-node view))
      (com.informatimago.common-lisp.picture.picture:draw-string
       pict x y (subtree-view-label view))
      (let ((line-x  (+ x (subtree-view-label-box-width view) -3))
            (above-y (+ y (max 2 (subtree-view-vertical-height-above view))))
            (below-y (- y (max 2 (subtree-view-vertical-height-below view)))))
        (com.informatimago.common-lisp.picture.picture:draw-line
         pict line-x below-y 0 (+ 1 
                                  (subtree-view-vertical-height-above view)
                                  (subtree-view-vertical-height-below view))
         :foreground (getf *line* :vertical))

        (apply (function com.informatimago.common-lisp.picture.picture:frame-rect)
               pict x (1- y) (subtree-view-label-box-width view) 3 *box*)
        (com.informatimago.common-lisp.picture.picture:draw-point
         pict x y (getf *line* :right-butt))
        (com.informatimago.common-lisp.picture.picture:draw-string
         pict (+ 2 x) y (subtree-view-label view))
        
        (com.informatimago.common-lisp.picture.picture:draw-line
         pict line-x above-y 5 0 :foreground (getf *line* :horizontal))
        (com.informatimago.common-lisp.picture.picture:draw-point
         pict line-x (1+ y)  (getf *line* :bottom-butt))
        (com.informatimago.common-lisp.picture.picture:draw-point
         pict line-x above-y (getf *line* :top-left))

        (com.informatimago.common-lisp.picture.picture:draw-line
         pict line-x below-y 5 0 :foreground (getf *line* :horizontal))
        (com.informatimago.common-lisp.picture.picture:draw-point
         pict line-x (1- y)  (getf *line* :top-butt))
        (com.informatimago.common-lisp.picture.picture:draw-point
         pict line-x below-y (getf *line* :bottom-left))
        
        (draw-tree-view (subtree-view-right-subtree-view view) (+ line-x 4) above-y pict)
        (draw-tree-view (subtree-view-left-subtree-view  view) (+ line-x 4) below-y pict)))
  pict)

(defun draw-tree (tree)
  (let* ((view (binary-tree-to-view tree))
         (pict (make-instance 'com.informatimago.common-lisp.picture.picture:picture
                   :width  (1+ (subtree-view-width  view))
                   :height (1+ (subtree-view-height view))))
         (x    0)
         (y    (subtree-view-height-below view)))
    (draw-tree-view view x y pict)
    pict))


;;;; THE END ;;;;
