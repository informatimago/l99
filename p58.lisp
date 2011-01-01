#-(and) "

P58 (**) Generate-and-test paradigm

    Apply the generate-and-test paradigm to construct all symmetric,
    completely balanced binary trees with a given number of
    nodes. Example:

    * sym-cbal-trees(5,Ts).

    Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)),
    t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]
   
    How many such trees are there with 57 nodes? Investigate about how
    many solutions there are for a given number of nodes? What if the
    number is even? Write an appropriate predicate.
"

(load "p54a")
(load "p55")
(load "p57")
(load "memoize")
(use-package :org.tfeb.hax.memoize)

(defun generate-balanced-binary-trees-of-height (height next-label)
  ;; When next-label is a constant function,
  ;; generate-balanced-binary-trees-of-height could be memoized.
  (case height
    (0 (list (make-empty-binary-tree)))
    (1 (list (make-binary-tree :label (funcall next-label))))
    (otherwise
     (let ((h-2 (generate-balanced-binary-trees-of-height (- height 2) next-label))
           (h-1 (generate-balanced-binary-trees-of-height (- height 1) next-label)))
       (nconc
        (mapcan (lambda (left)
                  (mapcar (lambda (right)
                            (make-binary-tree :label (funcall next-label)
                                              :left left
                                              :right right))
                          h-1))
                h-1)
        (mapcan (lambda (left)
                  (mapcar (lambda (right)
                            (make-binary-tree :label (funcall next-label)
                                              :left left
                                              :right right))
                          h-1))
                h-2)
        (mapcan (lambda (right)
                  (mapcar (lambda (left)
                            (make-binary-tree :label (funcall next-label)
                                              :left left
                                              :right right))
                          h-1))
                h-2))))))

(defun generate-binary-trees-of-nodes (number-of-nodes next-label)
  ;; When next-label is a constant function,
  ;; generate-binary-trees-of-nodes could be memoized.
  (case number-of-nodes
    (0 (list (make-empty-binary-tree)))
    (1 (list (make-binary-tree :label (funcall next-label))))
    (otherwise
     (let ((subtrees (loop
                        :with subtrees = (make-array number-of-nodes)
                        :for i :from 0 :below number-of-nodes
                        :do (setf (aref subtrees i) (generate-binary-trees-of-nodes i next-label))
                        :finally (return subtrees))))
       (loop
          :for left :from 0 :below number-of-nodes
          :for right = (- number-of-nodes left 1)
          :nconc (mapcan (lambda (left-subtree)
                           (mapcar (lambda (right-subtree)
                                     (make-binary-tree :label (funcall next-label)
                                                       :left left-subtree
                                                       :right right-subtree))
                                   (aref subtrees right)))
                         (aref subtrees left)))))))

(memoize-function 'generate-balanced-binary-trees-of-height)
(memoize-function 'generate-binary-trees-of-nodes)


(load "draw-tree")
#-(and)
(mapcar 'draw-tree
        (generate-binary-trees-of-nodes 5 (let ((n 0)) (lambda () (incf n)))))

#-(and) "
CL-USER> (mapcar 'draw-tree
                 (generate-binary-trees-of-nodes 3 (let ((n 0)) (lambda () (incf n)))))
(                             
                             
                             
              ┌─── nil       
            ╔═╧═╗            
        ┌───╢ 2 ║            
        │   ╚═╤═╝            
      ╔═╧═╗   └─── nil       
  ┌───╢ 3 ║                  
  │   ╚═╤═╝                  
╔═╧═╗   └─── nil             
╢ 5 ║                        
╚═╤═╝                        
  └─── nil                   

                              
                             
        ┌─── nil             
      ╔═╧═╗                  
  ┌───╢ 4 ║                  
  │   ╚═╤═╝   ┌─── nil       
  │     │   ╔═╧═╗            
  │     └───╢ 2 ║            
  │         ╚═╤═╝            
  │           └─── nil       
╔═╧═╗                        
╢ 6 ║                        
╚═╤═╝                        
  └─── nil                   

                      
                     
        ┌─── nil     
      ╔═╧═╗          
  ┌───╢ 1 ║          
  │   ╚═╤═╝          
╔═╧═╗   └─── nil     
╢ 7 ║                
╚═╤═╝   ┌─── nil     
  │   ╔═╧═╗          
  └───╢ 1 ║          
      ╚═╤═╝          
        └─── nil     
                     

                              
  ┌─── nil                   
╔═╧═╗                        
╢ 8 ║                        
╚═╤═╝                        
  │           ┌─── nil       
  │         ╔═╧═╗            
  │     ┌───╢ 2 ║            
  │     │   ╚═╤═╝            
  │   ╔═╧═╗   └─── nil       
  └───╢ 3 ║                  
      ╚═╤═╝                  
        └─── nil             
                             

                              
  ┌─── nil                   
╔═╧═╗                        
╢ 9 ║                        
╚═╤═╝   ┌─── nil             
  │   ╔═╧═╗                  
  └───╢ 4 ║                  
      ╚═╤═╝   ┌─── nil       
        │   ╔═╧═╗            
        └───╢ 2 ║            
            ╚═╤═╝            
              └─── nil       
                             
                             
)
"


(defun sym-cbal-trees (n)
  (remove-if-not (lambda (tree)
                   (and (binary-tree-symetric-p tree)
                        (binary-tree-balanced-p tree)))
                 (generate-binary-trees-of-nodes n (constantly 'x))))

#-(and)"

CL-USER> (mapcar 'draw-tree (sym-cbal-trees 7))
(                                    
        ┌─── nil                    
      ╔═╧═╗                         
  ┌───╢ X ║                         
  │   ╚═╤═╝   ┌─── nil              
  │     │   ╔═╧═╗                   
  │     └───╢ X ║                   
  │         ╚═╤═╝   ┌─── nil        
  │           │   ╔═╧═╗             
  │           └───╢ X ║             
  │               ╚═╤═╝             
  │                 └─── nil        
  │                                 
╔═╧═╗                               
╢ X ║                               
╚═╤═╝                               
  │                                 
  │                 ┌─── nil        
  │               ╔═╧═╗             
  │           ┌───╢ X ║             
  │           │   ╚═╤═╝             
  │         ╔═╧═╗   └─── nil        
  │     ┌───╢ X ║                   
  │     │   ╚═╤═╝                   
  │   ╔═╧═╗   └─── nil              
  └───╢ X ║                         
      ╚═╤═╝                         
        └─── nil                    
                                    

                                     
        ┌─── nil                    
      ╔═╧═╗                         
  ┌───╢ X ║                         
  │   ╚═╤═╝                         
  │     │           ┌─── nil        
  │     │         ╔═╧═╗             
  │     │     ┌───╢ X ║             
  │     │     │   ╚═╤═╝             
  │     │   ╔═╧═╗   └─── nil        
  │     └───╢ X ║                   
  │         ╚═╤═╝                   
  │           └─── nil              
╔═╧═╗                               
╢ X ║                               
╚═╤═╝                               
  │           ┌─── nil              
  │         ╔═╧═╗                   
  │     ┌───╢ X ║                   
  │     │   ╚═╤═╝   ┌─── nil        
  │     │     │   ╔═╧═╗             
  │     │     └───╢ X ║             
  │     │         ╚═╤═╝             
  │     │           └─── nil        
  │   ╔═╧═╗                         
  └───╢ X ║                         
      ╚═╤═╝                         
        └─── nil                    
                                    

                             
                            
              ┌─── nil      
            ╔═╧═╗           
        ┌───╢ X ║           
        │   ╚═╤═╝           
      ╔═╧═╗   └─── nil      
  ┌───╢ X ║                 
  │   ╚═╤═╝   ┌─── nil      
  │     │   ╔═╧═╗           
  │     └───╢ X ║           
  │         ╚═╤═╝           
  │           └─── nil      
╔═╧═╗                       
╢ X ║                       
╚═╤═╝                       
  │           ┌─── nil      
  │         ╔═╧═╗           
  │     ┌───╢ X ║           
  │     │   ╚═╤═╝           
  │   ╔═╧═╗   └─── nil      
  └───╢ X ║                 
      ╚═╤═╝   ┌─── nil      
        │   ╔═╧═╗           
        └───╢ X ║           
            ╚═╤═╝           
              └─── nil      
                            
                            

                                     
                                    
              ┌─── nil              
            ╔═╧═╗                   
        ┌───╢ X ║                   
        │   ╚═╤═╝   ┌─── nil        
        │     │   ╔═╧═╗             
        │     └───╢ X ║             
        │         ╚═╤═╝             
        │           └─── nil        
      ╔═╧═╗                         
  ┌───╢ X ║                         
  │   ╚═╤═╝                         
╔═╧═╗   └─── nil                    
╢ X ║                               
╚═╤═╝   ┌─── nil                    
  │   ╔═╧═╗                         
  └───╢ X ║                         
      ╚═╤═╝                         
        │           ┌─── nil        
        │         ╔═╧═╗             
        │     ┌───╢ X ║             
        │     │   ╚═╤═╝             
        │   ╔═╧═╗   └─── nil        
        └───╢ X ║                   
            ╚═╤═╝                   
              └─── nil              
                                    
                                    

                                     
                                    
                                    
                    ┌─── nil        
                  ╔═╧═╗             
              ┌───╢ X ║             
              │   ╚═╤═╝             
            ╔═╧═╗   └─── nil        
        ┌───╢ X ║                   
        │   ╚═╤═╝                   
      ╔═╧═╗   └─── nil              
  ┌───╢ X ║                         
  │   ╚═╤═╝                         
╔═╧═╗   └─── nil                    
╢ X ║                               
╚═╤═╝   ┌─── nil                    
  │   ╔═╧═╗                         
  └───╢ X ║                         
      ╚═╤═╝   ┌─── nil              
        │   ╔═╧═╗                   
        └───╢ X ║                   
            ╚═╤═╝   ┌─── nil        
              │   ╔═╧═╗             
              └───╢ X ║             
                  ╚═╤═╝             
                    └─── nil        
                                    
                                    
                                    
)
CL-USER>

"
;; (length (sym-cbal-trees 57))


;;;; THE END ;;;;
