
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defstruct sexp-scanner
;;   sexp
;;   current-cell
;;   stack)
;; 
;; ;; '(a and (not b)) --> a, and, "(", not, b, ")", $EOF
;; 
;; (defun scan-sexp (sexp-scanner)
;;   (if )
;;   )
;; 
;; 
;; (defun make-sexp-scanner (sexp)
;;   "Returns a scanner function")


(defun parse-term (iexpr)
  "
Parses:     term  := variable | constant | 'not' term | infix .
Returns the parsed expression converted to prefix, and the rest (unparsed tokens).
"
  (let ((token (first iexpr)))
   (cond
     ((eql token 'true)  (values token (rest iexpr)))
     ((eql token 'fail)  (values token (rest iexpr)))
     ((eql token 'not)
      (if (endp (rest iexpr))
          (error "Missing a term after 'not'")
          (multiple-value-bind (term rest) (parse-term (rest iexpr))
            (values `(not ,term) rest))))
     ((symbolp token)    (values token (rest iexpr)))
     ((atom token)       (error "Invalid atom ~A in the infix iexpr." iexpr))
     (t
      (multiple-value-bind (expr rest) (parse-infix token)
        (assert (endp rest) () "Remains unparsed tokens: ~A" rest)
        (values expr (rest iexpr)))))))


(defun parse-infix (iexpr)
  "
Parses:     infix := '(' term | term { op term } ')' .
            op    := 'and' | 'or' | 'nand' | 'nor' | 'xor' | 'impl' | 'equ' .
Returns the parsed expression converted to prefix, and the rest (unparsed tokens).
Note: In the returned prefix expression, all operators are binary, right associative.
"
  (assert (listp iexpr))
  (multiple-value-bind (left rest) (parse-term iexpr)
    (if (endp rest)
        (values left rest)
        (let ((op (first rest)))
          (case op
            ((and or nand nor xor impl equ)
             (multiple-value-bind (right rest) (parse-infix (rest rest))
               (values `(,op ,left ,right) rest)))
            (otherwise
             (error "Invalid operator '~A'" op)))))))


(defun infix-to-prefix (iexpr)
  "
term  := variable | constant | 'not' term | infix .
infix := '(' term | term { op term } ')' .
"
  (multiple-value-bind (expr rest) (parse-infix iexpr)
    (assert (endp rest) () "Remains unparsed tokens: ~A" rest)
    expr))


(defun test/infix-to-prefix ()
 (assert (equal
          (mapcar (lambda (iexpr) (handler-case (infix-to-prefix iexpr)
                                    (error (err) (princ err) (terpri) :error)))
                  '((a-variable)
                    (true)
                    (fail)
                    (not a-variable)
                    (not true)
                    (not fail)
                    (not not not true)
                    (a and b)
                    (a and b and c and d)
                    (a and b or not c and not d or e and not not f or g and h or not not not i)
                    ((a and b) or (c and d))
                    ((a or not (b and c)))
                    (a and (b or not c) and (not d or e) and (not not f or g) and (h or not not not i))
                    (a and 42)
                    (a + b)
                    (a and not)
                    (not a b)))
          '(A-VARIABLE
            TRUE
            FAIL
            (NOT A-VARIABLE)
            (NOT TRUE)
            (NOT FAIL)
            (NOT (NOT (NOT TRUE)))
            (AND A B)
            (AND A (AND B (AND C D)))
            (AND A (OR B (AND (NOT C) (OR (NOT D) (AND E (OR (NOT (NOT F)) (AND G (OR H (NOT (NOT (NOT I)))))))))))
            (OR (AND A B) (AND C D))
            (OR A (NOT (AND B C)))
            (AND A (AND (OR B (NOT C)) (AND (OR (NOT D) E) (AND (OR (NOT (NOT F)) G) (OR H (NOT (NOT (NOT I))))))))
            :ERROR :ERROR :ERROR :ERROR)))
 :success)

;; (test/infix-to-prefix)
;; --> :SUCCESS
