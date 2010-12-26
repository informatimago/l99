#-(and) "
   
P47 (*) Truth tables for logical expressions (2).

    Continue problem P46 by defining and/2, or/2, etc as being
    operators. This allows to write the logical expression in the more
    natural way, as in the example: A and (A or not B). Define
    operator precedence as usual; i.e. as in Java.
   
    Example:
    * table(A,B, A and (A or not B)).
    true true true
    true fail true
    fail true fail
    fail fail fail
"

;; Again, this question doesn't make sense in lisp.
;; To have fun, we could interpret it as requesting parsing a list in
;; infix notation and translating it to prefix notation.
;;
;; (infix-to-prefix '(a and (a or not b))) --> (and a (or a (not b)))
;;
;; but notice that we need a list anyways, and that parenthesized
;; subexpressions are actually sublists, there's no parentheses to be
;; parsed.
;;
;; On the other hand, we could write a full lexer and parser for
;; prolog syntax, but that would be out of scope for this exercise.
;;
;;
;; Java operator precedences are:
;;
;; Priority     Operators       Operation                       Associativity
;; 1            [ ]             array index                            left
;;              ()              method call
;;              .               member access
;; 2            ++              pre- or postfix increment              right
;;              --              pre- or postfix decrement
;;              + -             unary plus, minus
;;              ~               bitwise NOT
;;              !               boolean (logical) NOT
;;              (type)          type cast
;;              new             object creation
;; 3            * / %           multiplication, division, remainder    left
;; 4            + -             addition, substraction                 left
;;              +               string concatenation
;; 5            <<              signed bit shift left                  left
;;              >>              signed bit shift right
;;              >>>             unsigned bit shift right
;; 6            < <=            less than, less than or equal to       left
;;              > >=            greater than, greater than or equal to
;;              instanceof      reference test
;; 7            ==              equal to                               left
;;              !=              not equal to
;; 8            &               bitwise AND                            left
;;              &               boolean (logical) AND
;; 9            ^               bitwise XOR                            left
;;              ^               boolean (logical) XOR
;; 10           |               bitwise OR                             left
;;              |               boolean (logical) OR
;; 11           &&              boolean (logical) AND                  left
;; 12           ||              boolean (logical) OR                   left
;; 13           ? :             conditional                            right
;; 14           =               assignment                             right
;;              *= /= += -= %=
;;              <<= >>= >>>=
;;              &= ^= |=        combinated assignment
;;                              (operation and assignment)
;;
;; There are no NAND, NOR, EQU, or IMPL, but there are several AND and
;; OR, with different precedences!  What a fucking problem statement!
;;
;; So we will write a parser that is parameterized by the precedences,
;; and we'll see later what is needed.


(defparameter *operators* '(( 2 1 :left :prefix not)
                            ( 8 2 :left :infix  and nand)
                            ( 9 2 :left :infix  xor equ)
                            (10 2 :left :infix  or  nor)
                            (12 2 :left :infix  impl)))



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


;; (table 'a 'b (infix-to-prefix '(a and (a or not b))))
;; true true true
;; true fail true
;; fail true fail
;; fail fail fail
;; --> NIL


;;;; THE END ;;;;
