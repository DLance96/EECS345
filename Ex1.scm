; Programming Exercise 1
; Due Saturday, January 30th
; David Lance

; inorder? checks if input list is in chronological order
(define inorder?
  (lambda (ls)
    (cond
      ((null? ls) #t)
      ((null? (cdr ls)) #t)
      ((<= (car ls) (car (cdr ls))) (inorder? (cdr ls)))
      (else #f))))

; dotproduct takes a two vectors computes the dot product of the vectors
(define dotproduct
  (lambda (l1 l2)
    (cond
      ((null? l1) 0)
      ((null? l2) 0)
      (else (+ (* (car l1) (car l2)) (dotproduct (cdr l1) (cdr l2)))))))

; squareroot takes a value and iteration returns square root approx with formula
; Newton's method is new = old - ((old * old) - value) / (2 * old)
(define squareroot
  (lambda (val i)
    (cond
      ((eq? i 0) val)
      ((<= val 0) #f)
      (else (- (squareroot val (- i 1))
               (/ (- (* (squareroot val (- i 1))
                        (squareroot val (- i 1)))
                     val)
                  (* 2 (squareroot val (- i 1)))))))))

; removesubsequence takes two lists of atoms removes elements from first
; with the first occurance in the second list

(define removesubsequence
  (lambda (l1 l2)
    (cond
      ((null? l1) l2)
      ((null? l2) l2)
      ((eq? (car l1) (car l2)) (removesubsequence (cdr l1) (cdr l2)))
      (else (cons (car l2) (removesubsequence l1 (cdr l2)))))))

; reverse* reverses all the contents of a list including nested lists
(define reverse*
  (lambda (ls)
    (cond
      ((null? ls) ls)
      ((list? (car ls)) (append (reverse* (cdr ls)) (cons (reverse* (car ls)) '())))
      (else (append (reverse* (cdr ls)) (cons (car ls) '()))))))

; first* takes a list of lists and returns the first atom that appears in the list
(define first*
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((list? (car ls)) (first* (car ls)))
      (else (car ls)))))

; last* takes a list of lists and returns the last atom that appears
(define last*
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((and (null? (cdr ls)) (list? (car ls))) (last* (car ls)))
      ((null? (cdr ls)) (car ls))
      (else (last* (cdr ls))))))

; numorder? takes list returns true if values are increasing order, list may be nested
(define numorder*? 
  (lambda (l)
    (inorder? (runsum l))))

; HELPER
; runsum sums all nest lists in a list
(define runsum
  (lambda (l)
    (cond 
      ((null? l) '())
      ((list? (car l)) (cons (sumlst (car l)) (runsum (cdr l))))
      (else (cons (car l) (runsum (cdr l)))))))

; HELPER
; returns the sum of a list
(define sumlst
  (lambda (l)
    (cond
      ((null? l) 0)
      ((list? (car l)) (+ (sumlst (car l)) (sumlst (cdr l))))
      (else (+ (car l) (sumlst (cdr l)))))))

; vectormult dotproduct vector with each row of the matrix and return vector
(define vectormult
  (lambda (v mat)
    (cond
      ((null? v) '())
      ((null? mat) '())
      ((null? (car mat)) '())
      (else (cons (dotproduct v (carcolumn mat)) (vectormult v (cdrcolumn mat)))))))

; HELPER
; carcolumn returns the firt column of a matrix
(define carcolumn
  (lambda (mat)
    (cond
      ((null? mat) '())
      ((null? (car mat)))
      (else (cons (caar mat) (carcolumn (cdr mat)))))))

; HELPER
; cdrcolumn returns matrix without the first column
(define cdrcolumn
  (lambda (mat)
    (cond
      ((null? mat) '())
      (else (cons (cdar mat) (cdrcolumn (cdr mat)))))))
    
; matrixmultiply takes two matrixes and multiplies them
(define matrixmultiply
  (lambda (mat1 mat2)
    (cond
      ((null? mat1) '())
      ((null? mat2) '())
      ((null? (car mat1)) '())
      ((null? (car mat2)) '())
      (else (cons (vectormult (car mat1) mat2) (matrixmultiply (cdr mat1) mat2))))))
