#-(and)
(map nil 'print
     (sort (remove-if (lambda (p) (search "scratch" p))
                      (mapcar (function file-namestring) (directory "p*.lisp")))
           (function string<)))

(dolist (src '("p01.lisp" 
               "p02.lisp" 
               "p03.lisp" 
               "p04.lisp" 
               "p05.lisp" 
               "p06.lisp" 
               "p07.lisp" 
               "p08.lisp" 
               "p09.lisp" 
               "p10.lisp" 
               "p11.lisp" 
               "p12.lisp" 
               "p13.lisp" 
               "p14.lisp" 
               "p15.lisp" 
               "p16.lisp" 
               "p17.lisp" 
               "p18.lisp" 
               "p19.lisp" 
               "p20.lisp" 
               "p21.lisp" 
               "p22.lisp" 
               "p23.lisp" 
               "p24.lisp" 
               "p25.lisp" 
               "p26.lisp" 
               "p27.lisp" 
               "p28.lisp" 
               "p31.lisp" 
               "p32.lisp" 
               "p33.lisp" 
               "p34.lisp" 
               "p35.lisp" 
               "p36.lisp" 
               "p37.lisp" 
               "p38.lisp" 
               "p39.lisp" 
               "p40.lisp" 
               "p41.lisp" 
               "p46.lisp" 
               "rdp.lisp"     "p47.lisp" 
               "p48.lisp" 
               "memoize.lisp" "p49.lisp" 
               "p50.lisp" 
               "p54a.lisp" 
               "p55.lisp" 
               "p56.lisp" 
               "p57.lisp" 
               "draw-tree.lisp" "p58.lisp"
               "p61.lisp" 
               "p61a.lisp" 
               "p62.lisp" 
               "p62a.lisp" 
               "p63.lisp" 
               "p64.lisp" 
               "p65.lisp" 
               "p66.lisp" 
               "p67.lisp" 
               "p68.lisp" 
               "p69.lisp" 
               "p70b.lisp" 
               "p70c.lisp" 
               "p70.lisp" 
               "p71.lisp" 
               "p72.lisp" 
               "p73.lisp"
               ))
  (load (compile-file src)))
