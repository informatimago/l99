#+(and) "
P65 (**) Layout a binary tree (2)
    [p65]   

    An alternative layout method is depicted in the illustration
    opposite. Find out the rules and write the corresponding Prolog
    predicate. Hint: On a given level, the horizontal distance between
    neighboring nodes is constant.
   
    Use the same conventions as in problem P64 and test your predicate in an appropriate way.

"

(defun binary-tree-height (tree)
  (if (binary-tree-empty-p tree)
      0
      (+ 1 (max (binary-tree-height (binary-tree-left tree))
                (binary-tree-height (binary-tree-right tree))))))

(defun binary-tree-count-leftmosts (tree)
  (if (binary-tree-empty-p tree)
      0
      (+ 1 (binary-tree-count-leftmosts (binary-tree-left tree)))))


(defun layout-node-p65 (node abscissa depth height)
  "
The abscissa of the NODE is given by ABSCISSA, and the ordinate by DEPTH.
The abscissa of the children is offset by (expt 2 height).
"
  (setf (layout-binary-tree-x node) abscissa
        (layout-binary-tree-y node) depth)
  (let ((offset (expt 2 height)))
    (unless (binary-tree-empty-p (binary-tree-left node))
      (layout-node-p65 (binary-tree-left node)
                       (- abscissa offset)
                       (1+ depth)
                       (1- height)))
    (unless (binary-tree-empty-p (binary-tree-right node))
      (layout-node-p65 (binary-tree-right node)
                       (+ abscissa offset)
                       (1+ depth)
                       (1- height))))
  node)


(defun layout-binary-tree-p65 (tree)
  (let ((height (binary-tree-height tree)))
    (layout-node-p65 (binary-tree-to-layout-binary-tree tree)
                     (expt 2  height)
                     0
                     height)))

;;;; THE END ;;;;
